BUILDER
=======

Definição pela GOF(Gang of Four): "Separar a construção de um objeto complexo de sua representação para que o mesmo processo de
                                   construção possa criar representações diferentes."

No exemplo utilizado, foi modelado um processo de construção de dois diferentes tipos de pizza, no caso, Hawaiian Pizza e Spicy
Pizza. Para cada tipo de pizza foi utilizado um construtor específico, no caso HawaiianPizzaBuilder e SpicyPizzaBuilder. Cada
construtor monta uma diferente representação da Pizza. O processo de construcçaõ da pizza é o mesmo para ambas as pizzas, sendo
que o construtor é setado e a classe Waiter apenas chama o método constructPizza() para realizar a construção da pizza, de acordo
com o tipo de construtor da mesma, criando assim duas diferentes representações do objeto Pizza.
