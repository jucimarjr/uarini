SINGLETON
=========

Definição pela GOF(Gang of Four): "Garantir que uma classe só tenha uma única instância, e prover um ponto de acesso global a ela."

Para problemas em que exista a necessidade de garantir que apenas um objeto seja instanciado, evitando assim situações de erro de
comportamento, uso excessivo de recursos ou inconsistências, este padrão é o ideal. No exemplo mostrado, a classe que pode ser 
instanciada apenas uma vez é a ChocolateBoiler. E para controlar a quantidade de instanciações, é utilizada uma variável do tipo static
que sempre é verificada ao se tentar criar uma outra instância desta classe. O acesso global a esta única instância é feito por meio
do método getInstance() no Java, ou get_instance/0 no ooErlang, criando e retornando a única instância desta classe ou simplesmente
retornando a mesma se ela já tiver sido criada anteriormente. 
