STATE
=====

Definição pela GOF(Gang of Four): "Permitir a um objeto alterar o seu comportamento quando o seu estado interno mudar. O objeto
                                   irá aparentar mudar de classe."

No exemplo GumballMachine, é implementado um software para gerenciar o funcionamento de uma máquina de gomas de mascar. Para esta
modelagem, foram criados vários estados que esta máquina pode aparentar, sendo eles: Sem moeda (NoQuarterState), com moeda
(HasQuarterState), goma de mascar vendida (SoldState), sem goma de mascar (SoldOutState) e o estado atual da máquina (State). 
Cada estado foi modelado como uma classe, e ao ser instanciado, recebe um valor inicial. Para cada interação na máquina de gomas
de mascar, seu estado atual pode ou não sofrer alteração. Isto depende de duas coisas: O estado atual em que se encontra a máquina
e a ação que foi realizada (inserir moeda ou girar manivela). Se por exemplo a manivela for girada mas não houver moeda, nenhuma 
goma é retirada e o estado não muda. Mas se todas as gomas acabarem, o estado da máquina deverá mudar. Todos os estados foram 
implementados para responder às diferentes ações dos usuários.
