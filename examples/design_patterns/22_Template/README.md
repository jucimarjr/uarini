TEMPLATE
======

No desenvolvimento de sistemas de software, algumas vezes nos deparamos com problemas onde parte de um determinado algoritmo depende da implementação de classes filhas, mas a estrutura do algoritmo é única. Por exemplo, podemos ter uma classe que monta relatórios denominada Relatorio. 
O algoritmo para montar relatórios é sempre o mesmo consistindo nas seguintes operações:

• montar o cabeçalho
• montar o corpo
• montar o rodapé

Apesar da sequência de operações ser a mesma, os detalhes de implementação podem variar.
Relatórios para a mesma empresa podem manter o cabeçalho e o rodapé e variar o conteúdo do corpo.
Em outros cenários pode ser necessário variar também o cabeçalho e o rodapé. 
Como modelar as classes para atender os diversos cenários? 

