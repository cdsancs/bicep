// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
using System;
using System.Collections.Generic;
using System.Linq;
using Azure.Deployments.Expression.Configuration;
using Azure.Deployments.Expression.Expressions;
using Azure.Deployments.Expression.Serializers;
using Bicep.Core.Semantics;
using Bicep.Core.Syntax;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Bicep.Core.Emit
{
    public class ExpressionEmitter
    {
        private static readonly ExpressionSerializer ExpressionSerializer = new ExpressionSerializer(new ExpressionSerializerSettings
        {
            IncludeOuterSquareBrackets = true,

            // this setting will ensure that we emit strings instead of a string literal expressions
            SingleStringHandling = ExpressionSerializerSingleStringHandling.SerializeAsString
        });

        private readonly JsonTextWriter writer;
        private readonly EmitterContext context;
        private readonly ExpressionConverter converter;

        public ExpressionEmitter(JsonTextWriter writer, EmitterContext context)
        {
            this.writer = writer;
            this.context = context;
            this.converter = new ExpressionConverter(context);
        }

        public void EmitExpression(SyntaxBase syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            switch (syntax)
            {
                case BooleanLiteralSyntax boolSyntax:
                    writer.WriteValue(boolSyntax.Value);
                    break;

                case IntegerLiteralSyntax integerSyntax:
                    writer.WriteValue(integerSyntax.Value);
                    break;

                case NullLiteralSyntax _:
                    writer.WriteNull();

                    break;

                case ObjectSyntax objectSyntax:
                    writer.WriteStartObject();
                    EmitObjectProperties(objectSyntax, null, localVariableResolver);
                    writer.WriteEndObject();

                    break;

                case ArraySyntax arraySyntax:
                    writer.WriteStartArray();

                    foreach (ArrayItemSyntax itemSyntax in arraySyntax.Items)
                    {
                        EmitExpression(itemSyntax.Value, localVariableResolver);
                    }

                    writer.WriteEndArray();

                    break;

                case ParenthesizedExpressionSyntax _:
                case UnaryOperationSyntax _:
                case BinaryOperationSyntax _:
                case TernaryOperationSyntax _:
                case StringSyntax _:
                case InstanceFunctionCallSyntax _:
                case FunctionCallSyntax _:
                case ArrayAccessSyntax _:
                case PropertyAccessSyntax _:
                case VariableAccessSyntax _:
                    EmitLanguageExpression(syntax, localVariableResolver);
                    
                    break;

                default:
                    throw new NotImplementedException($"Cannot emit unexpected expression of type {syntax.GetType().Name}");
            }
        }

        public void EmitUnqualifiedResourceId(ResourceSymbol resourceSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var unqualifiedResourceId = converter.GetUnqualifiedResourceId(resourceSymbol, localVariableResolver);
            var serialized = ExpressionSerializer.SerializeExpression(unqualifiedResourceId);

            writer.WriteValue(serialized);
        }

        public void EmitResourceIdReference(ResourceSymbol resourceSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var resourceIdExpression = converter.GetLocallyScopedResourceId(resourceSymbol, localVariableResolver);
            var serialized = ExpressionSerializer.SerializeExpression(resourceIdExpression);

            writer.WriteValue(serialized);
        }

        public void EmitResourceIdReference(ModuleSymbol moduleSymbol, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var resourceIdExpression = converter.GetModuleResourceIdExpression(moduleSymbol, localVariableResolver);
            var serialized = ExpressionSerializer.SerializeExpression(resourceIdExpression);

            writer.WriteValue(serialized);
        }

        public LanguageExpression GetManagementGroupResourceId(SyntaxBase managementGroupNameProperty, bool fullyQualified, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => converter.GenerateManagementGroupResourceId(managementGroupNameProperty, fullyQualified, localVariableResolver);

        public void EmitLanguageExpression(SyntaxBase syntax, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var symbol = context.SemanticModel.GetSymbolInfo(syntax);
            if (symbol is VariableSymbol variableSymbol && context.VariablesToInline.Contains(variableSymbol))
            {
                EmitExpression(variableSymbol.Value, localVariableResolver);
                return;
            }

            if (syntax is FunctionCallSyntax functionCall && 
                symbol is FunctionSymbol functionSymbol && 
                string.Equals(functionSymbol.Name, "any", LanguageConstants.IdentifierComparison))
            {
                // the outermost function in the current syntax node is the "any" function
                // we should emit its argument directly
                // otherwise, they'd get wrapped in a json() template function call in the converted expression

                // we have checks for function parameter count mismatch, which should prevent an exception from being thrown
                EmitExpression(functionCall.Arguments.Single().Expression, localVariableResolver);
                return;
            }

            LanguageExpression converted = converter.ConvertExpression(syntax, localVariableResolver);

            if (converted is JTokenExpression valueExpression && valueExpression.Value.Type == JTokenType.Integer)
            {
                // the converted expression is an integer literal
                JToken value = valueExpression.Value;

                // for integer literals the expression will look like "[42]" or "[-12]"
                // while it's still a valid template expression that works in ARM, it looks weird
                // and is also not recognized by the template language service in VS code
                // let's serialize it as a proper integer instead
                writer.WriteValue(value);

                return;
            }

            // strings literals and other expressions must be processed with the serializer to ensure correct conversion and escaping
            var serialized = ExpressionSerializer.SerializeExpression(converted);

            writer.WriteValue(serialized);
        }

        public void EmitCopyObject(string name, ForSyntax syntax, SyntaxBase? input, string? copyIndexOverride, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            writer.WriteStartObject();

            this.EmitProperty("name", name);
            // construct the length ARM expression from the Bicep array expression
            // type check has already ensured that the array expression is an array
            this.EmitPropertyWithTransform(
                "count",
                syntax.Expression,
                arrayExpression => new FunctionExpression("length", new[] { arrayExpression }, Array.Empty<LanguageExpression>()),
                localVariableResolver);

            if (input != null)
            {
                if (copyIndexOverride == null)
                {
                    this.EmitProperty("input", input, localVariableResolver);
                }
                else
                {
                    this.EmitPropertyWithTransform("input", input, expression =>
                    {
                        // this is a giant hack
                        // the named copy index in the serialized expression is incorrect
                        // because the object syntax here does not match the JSON equivalent due to the presence of { "value": ... } wrappers
                        // for now, we will manually replace the copy index in the converted expression
                        // this also will not work for nested property loops
                        var visitor = new LanguageExpressionVisitor
                        {
                            OnFunctionExpression = function =>
                            {
                                if (string.Equals(function.Function, "copyIndex") &&
                                    function.Parameters.Length == 1 &&
                                    function.Parameters[0] is JTokenExpression)
                                {
                                    // it's an invocation of the copyIndex function with 1 argument with a literal value
                                    // replace the argument with the correct value
                                    function.Parameters = new LanguageExpression[] { new JTokenExpression("value") };
                                }
                            }
                        };

                        // mutate the expression
                        expression.Accept(visitor);

                        return expression;
                    },
                    localVariableResolver);
                }
            }

            writer.WriteEndObject();
        }

        public void EmitObjectProperties(ObjectSyntax objectSyntax, ISet<string>? propertiesToOmit, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            var propertyLookup = objectSyntax.Properties.ToLookup(property => property.Value is ForSyntax);

            // emit loop properties first (if any)
            if (propertyLookup.Contains(true))
            {
                // we have properties whose value is a for-expression
                this.EmitProperty("copy", () =>
                {
                    this.writer.WriteStartArray();

                    foreach (var property in propertyLookup[true])
                    {
                        var key = property.TryGetKeyText();
                        if (key is null || property.Value is not ForSyntax @for)
                        {
                            // should be caught by loop emit limitation checks
                            throw new InvalidOperationException("Encountered a property with an expression-based key whose value is a for-expression.");
                        }

                        this.EmitCopyObject(key, @for, @for.Body, null, localVariableResolver);
                    }

                    this.writer.WriteEndArray();
                });
            }

            // emit non-loop properties
            foreach (ObjectPropertySyntax propertySyntax in propertyLookup[false])
            {
                // property whose value is not a for-expression

                if (propertySyntax.TryGetKeyText() is string keyName)
                {
                    if (propertiesToOmit?.Contains(keyName) == true)
                    {
                        continue;
                    }

                    EmitProperty(keyName, propertySyntax.Value, localVariableResolver);
                }
                else
                {
                    EmitProperty(propertySyntax.Key, propertySyntax.Value, localVariableResolver);
                }
            }
        }

        public void EmitProperty(string name, LanguageExpression expressionValue)
            => EmitPropertyInternal(new JTokenExpression(name), () =>
            {
                var propertyValue = ExpressionSerializer.SerializeExpression(expressionValue);
                writer.WriteValue(propertyValue);
            });

        public void EmitPropertyWithTransform(string name, SyntaxBase value, Func<LanguageExpression, LanguageExpression> convertedValueTransform, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => EmitPropertyInternal(new JTokenExpression(name), () =>
            {
                var converted = converter.ConvertExpression(value, localVariableResolver);
                var transformed = convertedValueTransform(converted);
                var serialized = ExpressionSerializer.SerializeExpression(transformed);
                
                this.writer.WriteValue(serialized);
            });

        public void EmitProperty(string name, Action valueFunc)
            => EmitPropertyInternal(new JTokenExpression(name), valueFunc);

        public void EmitProperty(string name, string value)
            => EmitPropertyInternal(new JTokenExpression(name), value);

        public void EmitProperty(string name, SyntaxBase expressionValue, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => EmitPropertyInternal(new JTokenExpression(name), expressionValue, localVariableResolver);

        public void EmitProperty(SyntaxBase syntaxKey, SyntaxBase syntaxValue, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => EmitPropertyInternal(converter.ConvertExpression(syntaxKey, localVariableResolver), syntaxValue, localVariableResolver);

        private void EmitPropertyInternal(LanguageExpression expressionKey, Action valueFunc)
        {
            var serializedName = ExpressionSerializer.SerializeExpression(expressionKey);
            writer.WritePropertyName(serializedName);

            valueFunc();
        }

        private void EmitPropertyInternal(LanguageExpression expressionKey, string value)
            => EmitPropertyInternal(expressionKey, () =>
            {
                var propertyValue = ExpressionSerializer.SerializeExpression(new JTokenExpression(value));
                writer.WriteValue(propertyValue);
            });

        private void EmitPropertyInternal(LanguageExpression expressionKey, SyntaxBase syntaxValue, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
            => EmitPropertyInternal(expressionKey, () => EmitExpression(syntaxValue, localVariableResolver));

        public void EmitOptionalPropertyExpression(string name, SyntaxBase? expression, Func<LocalVariableSymbol, LanguageExpression>? localVariableResolver)
        {
            if (expression != null)
            {
                EmitProperty(name, expression, localVariableResolver);
            }
        }
    }
}

