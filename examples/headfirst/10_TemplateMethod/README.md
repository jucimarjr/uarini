TEMPLATE METHOD
===============

Definição pela GOF(Gang of Four): "Definir o esqueleto de um algoritmo dentro de uma operação, deixando alguns passos a serem
                                   preenchidos pelas subclasses. Template Method permite que suas subclasses redefinam certos 
                                   passos de um algoritmo sem mudar sua estrutura."

Neste exemplo é mostrada uma loja que prepara tanto café quanto chá. Dos passos realizados no preparo de cada um, observa-se que
alguns desses passos são iguais. Neste caso verifica-se que pode haver duplicação de código. Para evitar este tipo de situação,
foi utilizada a idéia do padrão Template Method para que fosse possível modelar os passos de preparação de café e chá que fossem 
iguais em apenas uma classe, que é a CaffeineBeverage. Evita-se assim a duplicação de código, simplificando-se a modelagem,
fazendo com que os passos de preparação do café e do chá que são iguais sejam tratados por apenas uma classe, e os passos diferentes
são preenchidos pelas classes específicas de cada tipo de bebida.
