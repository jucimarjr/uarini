//Este fonte foi retirado de: http://javapapers.com/design-patterns/mediator-design-pattern
//Ultimo acesso em Agosto de 2013

public class Flight implements Command {
	private IATCMediator atcMediator;

	public Flight(IATCMediator atcMediator) {
		this.atcMediator = atcMediator;
	}

	public void land() {
		if (atcMediator.isLandingOk()) {
			System.out.println("Landing done....");
			atcMediator.setLandingStatus(true);
		} else
			System.out.println("Will wait to land....");
	}

	public void getReady() {
		System.out.println("Getting ready...");
	}

}
