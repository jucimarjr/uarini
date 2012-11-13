MEMENTO
======

Memento é um padrão de projeto que permite armazenar o estado interno de um objeto em um determinado momento, para que seja possível retorná-lo a este estado, caso necessário. Três objetos estão envolvidos na implementação do padrão Memento.

	Originador é o objeto cujo estado se deseja capturar.
	Memento é o objeto definido dentro da classe Originador, com modificador de acesso privado, cujo estado do objeto originador será armazenado.
	Cliente é o objeto que acessará o originador, e deseja desfazer qualquer mudança efetuada, caso necessário.

O cliente deve requisitar um objeto memento, antes de se valer do originador. Após efetuar as operações desejadas no originador, o cliente devolve a este o objeto memento, caso deseje desfazer qualquer alteração. O objeto memento não permite o acesso de qualquer classe além da classe originador. Assim, tal padrão mostra-se útil por não violar o conceito de encapsulamento.
