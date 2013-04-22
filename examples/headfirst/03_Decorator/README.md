DECORATOR
=========

Definição pela GOF(Gang of Four): "Anexar responsabilidades adicionais a um objeto dinamicamente. Decorators oferecem uma 
                                   alternativa flexível ao uso de herança para estender uma funcionalidade."

No exemplo da Loja de cafés StarBuzzCoffee, cada tipo de café possui uma descrição de suas características e condimentos. Mas
dependendo do cliente, os condimentos de um tipo de café podem ser incrementados indefinidamente, e dinamicamente. Isto irá
implicar apenas em um custo adicional para cada novo condimento inserido em cada pedido. Para cada novo incremento, o objeto 
vai sendo "decorado", e seu custo vai sendo incrementado. Os tipos de decorators podem ser Milk, Whip, Soy e Mocha.
