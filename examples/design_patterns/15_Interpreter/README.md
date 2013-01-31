INTERPRETER
======

O padrão de projeto Interpreter pode ser utilizado para representar e resolver problemas recorrentes que possam ser expressos sob a forma de uma linguagem formal simples.
Gramáticas simples não precisam ser interpretadas por códigos criados através de geradores de analisadores sintáticos. Para isso, podemos criar uma simples hierarquia de classes baseada na gramática que, através de recursão, devolve a interpretação do código de entrada. É justamente isso que apresenta o padrão Interpreter: uma solução elegante na interpretação de pequenas gramáticas.
