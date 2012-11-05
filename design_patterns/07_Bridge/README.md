BRIDGE
======

Bridge é um padrão de projeto de software, ou design pattern em inglês, utilizado quando é desejável que uma interface (abstração) possa variar independentemente das suas implementações.

Imagine um sistema gráfico de janelas que deve ser portável para diversas plataformas. Neste sistema são encontrados diversos tipos de janelas, como ícones, diálogos, etc. Estas janelas formam uma hierarquia que contém uma abstração das janelas (classe base). Normalmente, a portabilidade seria obtida criando-se especializações dos tipos de janelas para cada uma das plataformas suportadas. O problema com essa solução reside na complexidade da hierarquia gerada e na dependência de plataforma que existirá nos clientes do sistema.

Através do padrão Bridge, a hierarquia que define os tipos de janelas é separada da hierarquia que contém a implementação. Desta forma todas as operações de Janela são abstratas e suas implementações são escondidas dos clientes.
