VISITOR
=======

Definição pela GOF(Gang of Four): "Representar uma operação a ser realizada sobre os elementos de uma estrutura de objetos. Visitor
                                   permite definir uma nova operação sem mudar as classes dos elementos nos quais opera."

No exemplo utilizado, temos uma composição de objetos que representarm as partes mecânicas e físicas de um carro (Wheel, Engine, Body,
Car). Neste cenário é necessário acessar cada um destes elementos da composição. Uma forma de resolver este problema seria modificando
o fonte de cada elemento da composição para que o mesmo tivesse um método que retornasse o estado interno do próprio elemento, ou que
realizasse uma operação neste elemento. Porém esta não é a melhor solução, tendo em vista que, para cada inserção de um novo elemento,
seria necessário refatorar o código novamente. Em casos semelhantes a esse é possível utilizar o padrão Visitor, em que uma classe
visitante acessa cada elemento da composição, realizando a tarefa necessária em cada um, sem que seja preciso alterar o fonte dos
elementos da composição. Neste exemplo foram implementados dois visitantes, CarElementPrintVisitor e CarElementDoVisitor, o primeiro
visita cada elemento do carro e o segundo realiza uma operação no elemento visitado.
