CHAIN OF RESPONSIBILITY
=======================

Definição pela GOF(Gang of Four): "Evita acoplar o remetente de uma requisição ao seu destinatário ao dar a mais de um objeto a
                                   chance de servir à requisição. Compõe os objetos em cascata e passa a requisição pela corrente
                                   até que um objeto a sirva."

Neste exemplo do padrão Chain of Responsibility, a requisição de entrada a ser processada é um valor numérico inteiro. Este valor
pode ser positivo, negativo ou nulo(zero). Para a criação da estrutura de cascata, foram criados três tipos de objetos que irão
compor a corrente, NegativeProcessor, PositiveProcessor e ZeroProcessor. Cada requisição de entrada irá ser servida por um desses
objetos. Se um objeto que processa a requisição não é o indicado, ele passa a responsabilidade para o próximo objeto da corrente, até
que um dos objetos sirva à requisição, de acordo com o padrão. 
