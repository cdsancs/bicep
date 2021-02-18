// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Azure.Deployments.Core.Extensions;
using Azure.Deployments.Expression.Expressions;
using Bicep.Core.Extensions;
using Bicep.Core.Resources;
using Bicep.Core.Semantics;
using Bicep.Core.Syntax;
using Bicep.Core.TypeSystem;
using Newtonsoft.Json.Linq;

namespace Bicep.Core.Emit
{
    public class ExpressionConverter
    {
        private readonly EmitterContext context;

        public ExpressionConverter(EmitterContext context)
        {
            this.context = context;
        }

        /// <summary>
        /// Converts the specified bicep expression tree into an ARM template expression tree.
        /// The returned tree may be rooted at either a function expression or jtoken expression.
        /// </summary>
        /// <param name="expression">The expression</param>
        public LanguageExpression ConvertExpression(SyntaxBase expression, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            switch (expression)
            {
                case BooleanLiteralSyntax boolSyntax:
                    return CreateFunction(boolSyntax.Value ? "true" : "false");

                case IntegerLiteralSyntax integerSyntax:
                    return integerSyntax.Value > int.MaxValue || integerSyntax.Value < int.MinValue ? CreateFunction("json", new JTokenExpression(integerSyntax.Value.ToInvariantString())) : new JTokenExpression((int)integerSyntax.Value);

                case StringSyntax stringSyntax:
                    // using the throwing method to get semantic value of the string because
                    // error checking should have caught any errors by now
                    return ConvertString(stringSyntax, localVariableResolver);

                case NullLiteralSyntax _:
                    return CreateFunction("null");

                case ObjectSyntax @object:
                    return ConvertObject(@object, localVariableResolver);

                case ArraySyntax array:
                    return ConvertArray(array, localVariableResolver);

                case ParenthesizedExpressionSyntax parenthesized:
                    // template expressions do not have operators so parentheses are irrelevant
                    return ConvertExpression(parenthesized.Expression, localVariableResolver);

                case UnaryOperationSyntax unary:
                    return ConvertUnary(unary, localVariableResolver);

                case BinaryOperationSyntax binary:
                    return ConvertBinary(binary, localVariableResolver);

                case TernaryOperationSyntax ternary:
                    return CreateFunction(
                        "if",
                        ConvertExpression(ternary.ConditionExpression, localVariableResolver),
                        ConvertExpression(ternary.TrueExpression, localVariableResolver),
                        ConvertExpression(ternary.FalseExpression, localVariableResolver));

                case FunctionCallSyntax function:
                    return ConvertFunction(
                        function.Name.IdentifierName,
                        function.Arguments.Select(a => ConvertExpression(a.Expression, localVariableResolver)));

                case InstanceFunctionCallSyntax instanceFunctionCall:
                    var namespaceSymbol = context.SemanticModel.GetSymbolInfo(instanceFunctionCall.BaseExpression);
                    Assert(namespaceSymbol is NamespaceSymbol, $"BaseExpression must be a NamespaceSymbol, instead got: '{namespaceSymbol?.Kind}'");

                    return ConvertFunction(
                        instanceFunctionCall.Name.IdentifierName,
                        instanceFunctionCall.Arguments.Select(a => ConvertExpression(a.Expression, localVariableResolver)));

                case ArrayAccessSyntax arrayAccess:
                    return ConvertArrayAccess(arrayAccess, localVariableResolver);

                case PropertyAccessSyntax propertyAccess:
                    return ConvertPropertyAccess(propertyAccess, localVariableResolver);

                case VariableAccessSyntax variableAccess:
                    return ConvertVariableAccess(variableAccess, localVariableResolver);

                default:
                    throw new NotImplementedException($"Cannot emit unexpected expression of type {expression.GetType().Name}");
            }
        }

        private LanguageExpression ConvertArrayAccess(ArrayAccessSyntax arrayAccess, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            // if there is an array access on a resource/module reference, we have to generate differently
            // when constructing the reference() function call, the resource name expression needs to have its local
            // variable replaced with <loop array expression>[this array access' index expression]
            if (arrayAccess.BaseExpression is VariableAccessSyntax variableAccess)
            {
                switch (this.context.SemanticModel.GetSymbolInfo(variableAccess))
                {
                    case ResourceSymbol {IsCollection: true}:
                        // TODO: Replace loop index correctly
                        return ToFunctionExpression(arrayAccess.BaseExpression, localVariableResolver);

                    case ModuleSymbol { IsCollection: true }:
                        // TODO: Replace loop index correctly
                        return ToFunctionExpression(arrayAccess.BaseExpression, localVariableResolver);
                }
            }

            return AppendProperties(
                ToFunctionExpression(arrayAccess.BaseExpression, localVariableResolver),
                ConvertExpression(arrayAccess.IndexExpression, localVariableResolver));
        }

        private LanguageExpression ConvertPropertyAccess(PropertyAccessSyntax propertyAccess, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            if (propertyAccess.BaseExpression is VariableAccessSyntax propVariableAccess &&
                context.SemanticModel.GetSymbolInfo(propVariableAccess) is ResourceSymbol resourceSymbol)
            {
                // special cases for certain resource property access. if we recurse normally, we'll end up
                // generating statements like reference(resourceId(...)).id which are not accepted by ARM

                var typeReference = EmitHelpers.GetSingleResourceTypeReference(resourceSymbol);
                switch (propertyAccess.PropertyName.IdentifierName)
                {
                    case "id":
                        return GetLocallyScopedResourceId(resourceSymbol, localVariableResolver);
                    case "name":
                        return GetResourceNameExpression(resourceSymbol, localVariableResolver);
                    case "type":
                        return new JTokenExpression(typeReference.FullyQualifiedType);
                    case "apiVersion":
                        return new JTokenExpression(typeReference.ApiVersion);
                    case "properties":
                        // use the reference() overload without "full" to generate a shorter expression
                        return GetReferenceExpression(resourceSymbol, typeReference, false, localVariableResolver);
                }
            }

            var moduleAccess = TryGetModulePropertyAccess(propertyAccess);
            if (moduleAccess != null)
            {
                var (moduleSymbol, outputName) = moduleAccess.Value;
                return AppendProperties(
                    GetModuleOutputsReferenceExpression(moduleSymbol, localVariableResolver),
                    new JTokenExpression(outputName),
                    new JTokenExpression("value"));
            }

            return AppendProperties(
                ToFunctionExpression(propertyAccess.BaseExpression, localVariableResolver),
                new JTokenExpression(propertyAccess.PropertyName.IdentifierName));
        }

        private (ModuleSymbol moduleSymbol, string outputName)? TryGetModulePropertyAccess(PropertyAccessSyntax propertyAccess)
        {
            // is this a (<child>.outputs).<prop> propertyAccess?
            if (propertyAccess.BaseExpression is PropertyAccessSyntax childPropertyAccess && childPropertyAccess.PropertyName.IdentifierName == LanguageConstants.ModuleOutputsPropertyName)
            {
                // is <child> a variable which points to a module symbol?
                if (childPropertyAccess.BaseExpression is VariableAccessSyntax grandChildVariableAccess && 
                    context.SemanticModel.GetSymbolInfo(grandChildVariableAccess) is ModuleSymbol {IsCollection: false} moduleSymbol)
                {
                    return (moduleSymbol, propertyAccess.PropertyName.IdentifierName);
                }

                // is <child> an array access operating on a module collection
                if (childPropertyAccess.BaseExpression is ArrayAccessSyntax grandChildArrayAccess &&
                    grandChildArrayAccess.BaseExpression is VariableAccessSyntax grandGrandChildVariableAccess &&
                    context.SemanticModel.GetSymbolInfo(grandGrandChildVariableAccess) is ModuleSymbol {IsCollection: true} moduleCollectionSymbol)
                {
                    return (moduleCollectionSymbol, propertyAccess.PropertyName.IdentifierName);
                }
            }

            return null;
        }

        

        private LanguageExpression GetResourceNameExpression(ResourceSymbol resourceSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            // this condition should have already been validated by the type checker
            var nameValueSyntax = resourceSymbol.SafeGetBodyPropertyValue(LanguageConstants.ResourceNamePropertyName) ?? throw new ArgumentException($"Expected resource syntax body to contain property 'name'");

            return ConvertExpression(nameValueSyntax, localVariableResolver);
        }

        private LanguageExpression GetModuleNameExpression(ModuleSymbol moduleSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            // this condition should have already been validated by the type checker
            var nameValueSyntax = moduleSymbol.SafeGetBodyPropertyValue(LanguageConstants.ResourceNamePropertyName) ?? throw new ArgumentException($"Expected module syntax body to contain property 'name'");
            return ConvertExpression(nameValueSyntax, localVariableResolver);
        }

        public IEnumerable<LanguageExpression> GetResourceNameSegments(ResourceSymbol resourceSymbol, ResourceTypeReference typeReference, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            if (typeReference.Types.Length == 1)
            {
                return GetResourceNameExpression(resourceSymbol, localVariableResolver).AsEnumerable();
            }

            return typeReference.Types.Select(
                (type, i) => AppendProperties(
                    CreateFunction(
                        "split",
                        GetResourceNameExpression(resourceSymbol, localVariableResolver),
                        new JTokenExpression("/")),
                    new JTokenExpression(i)));
        }

        private LanguageExpression GenerateScopedResourceId(ResourceSymbol resourceSymbol, ResourceScope? targetScope, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var typeReference = resourceSymbol.IsCollection
                ? EmitHelpers.GetResourceCollectionTypeReference(resourceSymbol)
                : EmitHelpers.GetSingleResourceTypeReference(resourceSymbol);
            var nameSegments = GetResourceNameSegments(resourceSymbol, typeReference, localVariableResolver);

            if (!context.ResourceScopeData.TryGetValue(resourceSymbol, out var scopeData))
            {
                return ScopeHelper.FormatLocallyScopedResourceId(targetScope, typeReference.FullyQualifiedType, nameSegments);
            }

            return ScopeHelper.FormatCrossScopeResourceId(this, scopeData, typeReference.FullyQualifiedType, nameSegments, localVariableResolver);
        }

        public LanguageExpression GetUnqualifiedResourceId(ResourceSymbol resourceSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => GenerateScopedResourceId(resourceSymbol, null, localVariableResolver);

        public LanguageExpression GetLocallyScopedResourceId(ResourceSymbol resourceSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => GenerateScopedResourceId(resourceSymbol, context.SemanticModel.TargetScope, localVariableResolver);

        public LanguageExpression GetModuleResourceIdExpression(ModuleSymbol moduleSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            return ScopeHelper.FormatCrossScopeResourceId(
                this,
                context.ModuleScopeData[moduleSymbol],
                TemplateWriter.NestedDeploymentResourceType,
                GetModuleNameExpression(moduleSymbol, localVariableResolver).AsEnumerable(),
                localVariableResolver);
        }

        public FunctionExpression GetModuleOutputsReferenceExpression(ModuleSymbol moduleSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => AppendProperties(
                CreateFunction(
                    "reference",
                    GetModuleResourceIdExpression(moduleSymbol, localVariableResolver),
                    new JTokenExpression(TemplateWriter.NestedDeploymentResourceApiVersion)),
                new JTokenExpression("outputs"));

        public FunctionExpression GetReferenceExpression(ResourceSymbol resourceSymbol, ResourceTypeReference typeReference, bool full, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            // full gives access to top-level resource properties, but generates a longer statement
            if (full)
            {
                return CreateFunction(
                    "reference",
                    GetLocallyScopedResourceId(resourceSymbol, localVariableResolver),
                    new JTokenExpression(typeReference.ApiVersion),
                    new JTokenExpression("full"));
            }

            if (resourceSymbol.DeclaringResource.IsExistingResource())
            {
                // we must include an API version for an existing resource, because it cannot be inferred from any deployed template resource
                return CreateFunction(
                    "reference",
                    GetLocallyScopedResourceId(resourceSymbol, localVariableResolver),
                    new JTokenExpression(typeReference.ApiVersion));
            }

            return CreateFunction(
                "reference",
                GetLocallyScopedResourceId(resourceSymbol, localVariableResolver));
        }

        private LanguageExpression GetLocalVariableExpression(LocalVariableSymbol localVariableSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var parent = this.context.SemanticModel.Binder.GetParent(localVariableSymbol.DeclaringLocalVariable);

            switch (parent)
            {
                case ForSyntax @for when ReferenceEquals(@for.ItemVariable, localVariableSymbol.DeclaringLocalVariable):
                    // this is the "item" variable of a for-expression
                    // to emit this we need to basically index the array expression by the copyIndex() function
                    var arrayExpression = ToFunctionExpression(@for.Expression, localVariableResolver);

                    var copyIndexName = this.context.SemanticModel.Binder.GetParent(@for) switch
                    {
                        // copyIndex without name resolves to module/resource loop index in the runtime
                        ResourceDeclarationSyntax => null,
                        ModuleDeclarationSyntax => null,

                        ObjectPropertySyntax property when property.TryGetKeyText() is { } key && ReferenceEquals(property.Value, @for) => key,

                        _ => throw new NotImplementedException("Unexpected for-expression grandparent.")
                    };

                    var copyIndexFunction = copyIndexName == null ? CreateFunction("copyIndex") : CreateFunction("copyIndex", new JTokenExpression(copyIndexName));
                    
                    return AppendProperties(arrayExpression, copyIndexFunction);

                default:
                    throw new NotImplementedException($"Encountered a local variable with parent of unexpected type '{parent?.GetType().Name}'.");
            }
        }

        private LanguageExpression ConvertVariableAccess(VariableAccessSyntax variableAccessSyntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            string name = variableAccessSyntax.Name.IdentifierName;

            var symbol = context.SemanticModel.GetSymbolInfo(variableAccessSyntax);

            // TODO: This will change to support inlined functions like reference() or list*()
            switch (symbol)
            {
                case ParameterSymbol _:
                    return CreateFunction("parameters", new JTokenExpression(name));

                case VariableSymbol variableSymbol:
                    if (context.VariablesToInline.Contains(variableSymbol))
                    {
                        // we've got a runtime dependency, so we have to inline the variable usage
                        return ConvertExpression(variableSymbol.DeclaringVariable.Value, localVariableResolver);
                    }
                    return CreateFunction("variables", new JTokenExpression(name));

                case ResourceSymbol resourceSymbol:
                    var typeReference = resourceSymbol.IsCollection
                        ? EmitHelpers.GetResourceCollectionTypeReference(resourceSymbol)
                        : EmitHelpers.GetSingleResourceTypeReference(resourceSymbol);
                    return GetReferenceExpression(resourceSymbol, typeReference, true, localVariableResolver);

                case ModuleSymbol moduleSymbol:
                    return GetModuleOutputsReferenceExpression(moduleSymbol, localVariableResolver);

                case LocalVariableSymbol localVariableSymbol:
                    return GetLocalVariableExpression(localVariableSymbol, localVariableResolver);

                default:
                    throw new NotImplementedException($"Encountered an unexpected symbol kind '{symbol?.Kind}' when generating a variable access expression.");
            }
        }

        private LanguageExpression ConvertString(StringSyntax syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            if (syntax.TryGetLiteralValue() is string literalStringValue)
            {
                // no need to build a format string
                return new JTokenExpression(literalStringValue);
            }

            if (syntax.Expressions.Length == 1)
            {
                const string emptyStringOpen = LanguageConstants.StringDelimiter + LanguageConstants.StringHoleOpen; // '${
                const string emptyStringClose = LanguageConstants.StringHoleClose + LanguageConstants.StringDelimiter; // }'

                // Special-case interpolation of format '${myValue}' because it's a common pattern for userAssignedIdentities.
                // There's no need for a 'format' function because we just have a single expression with no outer formatting.
                if (syntax.StringTokens[0].Text == emptyStringOpen && syntax.StringTokens[1].Text == emptyStringClose)
                {
                    return ConvertExpression(syntax.Expressions[0], localVariableResolver);
                }
            }

            var formatArgs = new LanguageExpression[syntax.Expressions.Length + 1];

            var formatString = StringFormatConverter.BuildFormatString(syntax);
            formatArgs[0] = new JTokenExpression(formatString);

            for (var i = 0; i < syntax.Expressions.Length; i++)
            {
                formatArgs[i + 1] = ConvertExpression(syntax.Expressions[i], localVariableResolver);
            }

            return CreateFunction("format", formatArgs);
        }

        /// <summary>
        /// Converts the specified bicep expression tree into an ARM template expression tree.
        /// This always returns a function expression, which is useful when converting property access or array access
        /// on literals.
        /// </summary>
        /// <param name="expression">The expression</param>
        public FunctionExpression ToFunctionExpression(SyntaxBase expression, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var converted = ConvertExpression(expression, localVariableResolver);
            switch (converted)
            {
                case FunctionExpression functionExpression:
                    return functionExpression;

                case JTokenExpression valueExpression:
                    JToken value = valueExpression.Value;

                    switch (value.Type)
                    {
                        case JTokenType.Integer:
                            // convert integer literal to a function call via int() function
                            return CreateFunction("int", valueExpression);

                        case JTokenType.String:
                            // convert string literal to function call via string() function
                            return CreateFunction("string", valueExpression);
                    }

                    break;
            }

            throw new NotImplementedException($"Unexpected expression type '{converted.GetType().Name}'.");
        }

        private static LanguageExpression ConvertFunction(string functionName, IEnumerable<LanguageExpression> arguments)
        {
            if (string.Equals("any", functionName, LanguageConstants.IdentifierComparison))
            {
                // this is the any function - don't generate a function call for it
                return arguments.Single();
            }

            if (ShouldReplaceUnsupportedFunction(functionName, arguments, out var replacementExpression))
            {
                return replacementExpression;
            }

            return CreateFunction(functionName, arguments);
        }

        private static bool ShouldReplaceUnsupportedFunction(string functionName, IEnumerable<LanguageExpression> arguments, [NotNullWhen(true)] out LanguageExpression? replacementExpression)
        {
            switch (functionName)
            {
                // These functions have not yet been implemented in ARM. For now, we will just return an empty object if they are accessed directly.
                case "tenant":
                case "managementGroup":
                case "subscription" when arguments.Any():
                case "resourceGroup" when arguments.Any():
                    replacementExpression = GetCreateObjectExpression();
                    return true;
            }

            replacementExpression = null;
            return false;
        }

        private FunctionExpression ConvertArray(ArraySyntax syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            // we are using the createArray() function as a proxy for an array literal
            return CreateFunction(
                "createArray",
                syntax.Items.Select(item => ConvertExpression(item.Value, localVariableResolver)));
        }

        private FunctionExpression ConvertObject(ObjectSyntax syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            // need keys and values in one array of parameters
            var parameters = new LanguageExpression[syntax.Properties.Count() * 2];

            int index = 0;
            foreach (var propertySyntax in syntax.Properties)
            {
                parameters[index] = propertySyntax.Key switch
                {
                    IdentifierSyntax identifier => new JTokenExpression(identifier.IdentifierName),
                    StringSyntax @string => ConvertString(@string, localVariableResolver),
                    _ => throw new NotImplementedException($"Encountered an unexpected type '{propertySyntax.Key.GetType().Name}' when generating object's property name.")
                };
                index++;

                parameters[index] = ConvertExpression(propertySyntax.Value, localVariableResolver);
                index++;
            }

            // we are using the createObject() funciton as a proy for an object literal
            return GetCreateObjectExpression(parameters);
        }

        private static FunctionExpression GetCreateObjectExpression(params LanguageExpression[] parameters)
            => CreateFunction("createObject", parameters);

        private LanguageExpression ConvertBinary(BinaryOperationSyntax syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            LanguageExpression operand1 = ConvertExpression(syntax.LeftExpression, localVariableResolver);
            LanguageExpression operand2 = ConvertExpression(syntax.RightExpression, localVariableResolver);

            switch (syntax.Operator)
            {
                case BinaryOperator.LogicalOr:
                    return CreateFunction("or", operand1, operand2);

                case BinaryOperator.LogicalAnd:
                    return CreateFunction("and", operand1, operand2);

                case BinaryOperator.Equals:
                    return CreateFunction("equals", operand1, operand2);

                case BinaryOperator.NotEquals:
                    return CreateFunction("not",
                        CreateFunction("equals", operand1, operand2));

                case BinaryOperator.EqualsInsensitive:
                    return CreateFunction("equals",
                        CreateFunction("toLower", operand1),
                        CreateFunction("toLower", operand2));

                case BinaryOperator.NotEqualsInsensitive:
                    return CreateFunction("not",
                        CreateFunction("equals",
                            CreateFunction("toLower", operand1),
                            CreateFunction("toLower", operand2)));

                case BinaryOperator.LessThan:
                    return CreateFunction("less", operand1, operand2);

                case BinaryOperator.LessThanOrEqual:
                    return CreateFunction("lessOrEquals", operand1, operand2);

                case BinaryOperator.GreaterThan:
                    return CreateFunction("greater", operand1, operand2);

                case BinaryOperator.GreaterThanOrEqual:
                    return CreateFunction("greaterOrEquals", operand1, operand2);

                case BinaryOperator.Add:
                    return CreateFunction("add", operand1, operand2);

                case BinaryOperator.Subtract:
                    return CreateFunction("sub", operand1, operand2);

                case BinaryOperator.Multiply:
                    return CreateFunction("mul", operand1, operand2);

                case BinaryOperator.Divide:
                    return CreateFunction("div", operand1, operand2);

                case BinaryOperator.Modulo:
                    return CreateFunction("mod", operand1, operand2);

                case BinaryOperator.Coalesce:
                    return CreateFunction("coalesce", operand1, operand2);

                default:
                    throw new NotImplementedException($"Cannot emit unexpected binary operator '{syntax.Operator}'.");
            }
        }

        private LanguageExpression ConvertUnary(UnaryOperationSyntax syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            LanguageExpression convertedOperand = ConvertExpression(syntax.Expression, localVariableResolver);

            switch (syntax.Operator)
            {
                case UnaryOperator.Not:
                    return CreateFunction("not", convertedOperand);

                case UnaryOperator.Minus:
                    if (convertedOperand is JTokenExpression literal && literal.Value.Type == JTokenType.Integer)
                    {
                        // invert the integer literal
                        int literalValue = literal.Value.Value<int>();
                        return new JTokenExpression(-literalValue);
                    }

                    return CreateFunction(
                        "sub",
                        new JTokenExpression(0),
                        convertedOperand);

                default:
                    throw new NotImplementedException($"Cannot emit unexpected unary operator '{syntax.Operator}.");
            }
        }

        public static LanguageExpression GenerateUnqualifiedResourceId(string fullyQualifiedType, IEnumerable<LanguageExpression> nameSegments)
        {
            var typeSegments = fullyQualifiedType.Split("/");

            // Generate a format string that looks like: My.Rp/type1/{0}/type2/{1}
            var formatString = $"{typeSegments[0]}/" + string.Join('/', typeSegments.Skip(1).Select((type, i) => $"{type}/{{{i}}}"));

            return CreateFunction(
                "format",
                new JTokenExpression(formatString).AsEnumerable().Concat(nameSegments));
        }

        public static LanguageExpression GenerateScopedResourceId(LanguageExpression scope, string fullyQualifiedType, IEnumerable<LanguageExpression> nameSegments)
            => CreateFunction(
                "extensionResourceId",
                new[] { scope, new JTokenExpression(fullyQualifiedType), }.Concat(nameSegments));

        public static LanguageExpression GenerateResourceGroupScope(LanguageExpression subscriptionId, LanguageExpression resourceGroup)
            => CreateFunction(
                "format",
                new JTokenExpression("/subscriptions/{0}/resourceGroups/{1}"),
                subscriptionId,
                resourceGroup);

        public static LanguageExpression GenerateTenantResourceId(string fullyQualifiedType, IEnumerable<LanguageExpression> nameSegments)
            => CreateFunction(
                "tenantResourceId",
                new[] { new JTokenExpression(fullyQualifiedType), }.Concat(nameSegments));

        public LanguageExpression GenerateManagementGroupResourceId(SyntaxBase managementGroupNameProperty, bool fullyQualified, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            const string managementGroupType = "Microsoft.Management/managementGroups";
            var managementGroupName = ConvertExpression(managementGroupNameProperty, localVariableResolver);

            if (fullyQualified)
            {
                return GenerateTenantResourceId(managementGroupType, new[] { managementGroupName });
            }
            else
            {
                return GenerateUnqualifiedResourceId(managementGroupType, new[] { managementGroupName });
            }
        }

        private static FunctionExpression CreateFunction(string name, params LanguageExpression[] parameters)
            => CreateFunction(name, parameters as IEnumerable<LanguageExpression>);

        private static FunctionExpression CreateFunction(string name, IEnumerable<LanguageExpression> parameters)
            => new FunctionExpression(name, parameters.ToArray(), Array.Empty<LanguageExpression>());

        private static FunctionExpression AppendProperties(FunctionExpression function, params LanguageExpression[] properties)
            => AppendProperties(function, properties as IEnumerable<LanguageExpression>);

        private static FunctionExpression AppendProperties(FunctionExpression function, IEnumerable<LanguageExpression> properties)
            => new FunctionExpression(function.Function, function.Parameters, function.Properties.Concat(properties).ToArray());

        protected static void Assert(bool predicate, string message)
        {
            if (predicate == false)
            {
                // we have a code defect - use the exception stack to debug
                throw new ArgumentException(message);
            }
        }
    }
}

