MEMENTO
=======

Definição pela GOF(Gang of Four): "Sem violar o encapsulamento, capturar e externalizar o estado interno de um objeto para que
                                   o objeto possa ter esse estado restaurado posteriormente."

O exemplo dado utiliza o padrão Memento criando estados e salvando alguns deles, para em seguida restaurar os estados que foram
salvos anteriormente. Existem duas classes principais: Originator, responsável por gerar os diferentes estados e por restaurar
um estado já salvo anteriormente. Caretaker, responsável por salvar o estado atual. Caretaker também possui uma lista de todos
os estados já salvos. A classe chamada Memento é de fato o estado salvo pelo Caretaker. Dessa forma é possível salvar diferentes
estados, e retornar aos estados salvos anteriormente, sem violar o encapsulamento dos estados salvos, conforme a definição do padrão.
