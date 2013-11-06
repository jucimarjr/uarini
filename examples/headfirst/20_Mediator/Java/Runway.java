//Este fonte foi retirado de: http://javapapers.com/design-patterns/mediator-design-pattern
//Ultimo acesso em Agosto de 2013

public class Runway implements Command {
	private IATCMediator atcMediator;

	public Runway(IATCMediator atcMediator) {
		this.atcMediator = atcMediator;
		atcMediator.setLandingStatus(false);
	}

	public void land() {
		System.out.println("Landing permission granted...");
		atcMediator.setLandingStatus(true);
	}
}
