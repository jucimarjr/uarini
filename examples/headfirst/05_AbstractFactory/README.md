ABSTRACT FACTORY
================

Definição pela GOF(Gang of Four): "Prover uma interface para criar famílias de objetos relacionados ou dependentes sem especificar
                                   suas classes concretas."

Neste exemplo, o cenário de duas lojas de Pizza (NYPizzaStore e ChicagoPizzaStore) é novamente utilizado. Mas neste caso, a quantidade
de ingredientes é maior. E para modelar da melhor forma este cenário, são criadas famílias de objetos para cada tipo de ingrediente 
das pizzas. As famílias de ingredientes criadas são "Veggies", para definir os tipos de vegetais, "Clams", para definir o tipo de 
molusco, se houver, "Sauce", para definir o tipo de molho da pizza, "Cheese", para o tipo de queijo, "Pepperoni", para o tipo de 
pepperoni, e "Dough", para definir o tipo de massa da pizza. Dessa forma existe uma interface para cada família de objetos, e esta 
interface é implementada pelos diversos tipos de objetos que a mesma representa.
