STRATEGY
========

Definição pela GOF(Gang of Four): "Definir uma família de algoritmos, encapsular cada um, e fazê-los intercambiáveis.
								   Strategy permite que algoritmos mudem independentemente entre clientes que o utilizam."

No exemplo do Simulador de Patos, existem duas características para cada um: Voar e Grasnar (Fly, Quack). Para cada tipo de
pato diferente existe um comportamento diferente para estes comportamentos. Uns voam, outros não. Uns fazem "Quack", outros
"Squeak" e outros ficam mudos. Como estes comportamentos irão variar para cada pato, eles foram isolados da classe "Duck". 
Essa é a idéia do Strategy neste exemplo: Isolar o que varia (comportamentos) do resto do código (encapsular). Desta forma 
sempre que um novo comportamento de "Fly" ou "Quack" for inserido, isto não irá impactar no comportamento dos Patos já existentes.
A família de algoritmos refere-se aos diferentes comportamentos de cada pato.
