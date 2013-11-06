//Este fonte foi retirado de: http://javapapers.com/design-patterns/mediator-design-pattern
//Ultimo acesso em Agosto de 2013

public interface IATCMediator {

	public void registerRunway(Runway runway);

	public void registerFlight(Flight flight);

	public boolean isLandingOk();

	public void setLandingStatus(boolean status);
}
