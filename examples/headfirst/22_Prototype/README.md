PROTOTYPE
=========

Definição pela GOF(Gang of Four): "Especificar os tipos de objetos a serem criados usando uma instância como protótipo e criar
                                   novos objetos ao copiar este protótipo."

No exemplo utilizado, existem dois tipos diferentes de objetos, Pessoa (Person) e Cachorro (Dog). O padrão Prototype é utilizado
quando é necessário obter um novo objeto, e para isso é feita uma cópia de uma instância já existente. Este padrão é útil em situações
onde a criação de um novo objeto, geralmente complexo, é muito custosa, sendo melhor "clonar" um objeto parecido com o que se deseja
obter ao invés de criar um novo objeto, modificando apenas alguns aspectos do objeto clonado. Desta forma, neste exemplo, ambos os
objetos são clonados (Person, Dog), criando-se assim novos objetos de cada um dos objetos clonados, parecidos com os que já existiam.
