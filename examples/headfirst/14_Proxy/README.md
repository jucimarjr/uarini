PROXY
=====

Definição pela GOF(Gang of Four): "Prover um substituto ou ponto de acesso do qual um objeto possa controlar o acesso a outro."

Neste exemplo, é implementado um carregador de imagens simplificado. Toda vez que uma imagem é requisitada para ser visualizada,
considerando que a imagem está em um outro computador, ou em um servidor, é necessário carregar a imagem para que a visualização
seja possível. Com a implementação do ProxyImage, o proxy vai carregar a imagem na primeira vez em que sua visualização for
requisitada, salvando esse carregamento para que, posteriormente, em futuras requisições de visualização, não seja mais necessário
carregar a imagem, que já vai estar carregada. Neste caso a classe ProxyImage controla o acesso à class RealImage, de acordo com
a definição do padrão. Existem vários tipos diferentes de Proxy, este é um exemplo do tipo "Virtual Proxy".
