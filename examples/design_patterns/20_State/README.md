STATE
=====

*Conhecido também como Objects for States


O design pattern State permite que parte do comportamento de um objeto seja alterado conforme o estado do
objeto. Sabemos que todo objeto possui atributos, que representam seu estado, e também métodos, que
representam seu comportamento.


Podemos facilmente imaginar situações onde programamos o comportamento do objeto conforme seu estado, por
exemplo, imagine que em um objeto Pedido, o método cancelar pode variar de acordo com o estado do pedido: se
o pedido estiver com estado de Faturado, o cancelamento implicará em cancelar faturamento e nota fiscal,
devolver para o estoque, cancelar solicitação de entrega, etc. Agora se o pedido estiver com estado de Entregue,
então o cancelamento implicará em processo de devolução / retirada dos produtos, e mais os processos de
cancelamento de fatura.


Normalmente sem o uso de State programaríamos o método cancelar com uma porção de if’s para tramitar o
processo conforme o estado do pedido. State propõe a criação de um modelo de classes que representam
polimorficamente, conforme o estado, uma porção do comportamento de um objeto. Ao mudar o estado do objeto,
mudamos parte do seu comportamento, ou todo ele se necessário.


Geralmente somente um estado significativo de um objeto alterara a forma que o objeto se comporta, por
exemplo, alterar a razão social de um objeto empresa, não necessariamente fará com que a empresa passe a se
comportar diferente, mas alterar o estado de um suposto objeto Email de “A enviar” para “Enviado” pode fazer
com que seu comportamento mude bastante, portanto esta é uma mudança significativa de estado de objeto.
