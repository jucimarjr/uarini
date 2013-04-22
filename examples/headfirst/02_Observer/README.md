OBSERVER
========

Definição pela GOF(Gang of Four): "Definir uma dependência um-para-muitos entre objetos para que quando um objeto mudar de estado,
                                   todos os seus dependentes sejam notificados e atualizados automaticamente."

No exemplo da estação meteorológica, existem quatro observadores, no caso, HeatIndexDisplay, ForecastDisplay, StatisticsDisplay e
CurrentConditionsDisplay. Cada um mostra um aspecto da medição meteorológica. Para cada mudança de Temperatura, Humidade ou Pressão,
Todos esses observadores são notificados pelo objeto do tipo WeatherData, responsável por notificar os observadores quando há mudança
em algum desses fatores meteorológicos. Dessa forma, todas as medições são atualizadas automaticamente, de acordo com o padrão.
