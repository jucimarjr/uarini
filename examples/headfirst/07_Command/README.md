COMMAND
=======

Definição pela GOF(Gang of Four): "Encapsular uma requisição como um objeto, permitindo que clientes parametrizem diferentes
                                   requisições, filas ou requisições de log, e suportar operações reversíveis."

No exemplo foi desenvolvida uma API para controlar os comandos de um controle remoto. Foi criada a interface Command, que apenas
executa o comando passado a ela. A estrutura deste padrão possibilita que um determinado comando, como por exemplo, ligar ou
desligar uma luz, possa ser parametrizado para ser utilizado na classe SimpleRemoteControl, fazendo parte de um de seus atributos.
Desta forma o comando em si fica encapsulado e é executado simplesmente com a chamada do método execute(), que realiza a ação
atribuída a certo botão do controle remoto. A classe SimpleRemoteControl não se preocupa em qual comando irá executar, apenas
executa o comando que foi atribuído a um de seus botões.
