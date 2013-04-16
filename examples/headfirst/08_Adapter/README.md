ADAPTER
=======

Definição pela GOF(Gang of Four): "O padrão Adapter converte a interface de uma classe em outra interface esperada pelos clientes.
                                   Adapter permite a comunicação entre classes que não poderiam trabalhar juntas devido à 
                                   incompatibilidade de suas interfaces."

Neste exemplo voltamos novamente ao Simulador de patos, em uma versão simplificada. Os patos possuem os comportamentos fly() e 
quack(). Neste caso existe outro tipo de animal, o peru (Turkey). Seus comportamentos são gobble() e fly(), e não são compatíveis
com os dos patos. O objetivo é fazer com que os patos possam utilizar a interface dos perus e vice-versa. Para isso, são criados
dois adaptadores, o DuckAdapter, que permita aos patos utilizarem a interface dos perus e o TurkeyAdapter, para que os perus 
utilizem a interface dos patos. Desta forma não foi preciso modificar as classes Duck e Turkey, apenas foi necessário criar os
Adaptadores para que as classes pudessem utilizar as interfaces uma da outra.
