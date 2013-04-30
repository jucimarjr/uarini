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
