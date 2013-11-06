//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class Tuner {
	String description;
	Amplifier amplifier;
	double frequency;
	
	public Tuner(String description, Amplifier amplifier) {
		this.description = description;
	}
 
	public void on() {
		System.out.println(description + " on");
	}
 
	public void off() {
		System.out.println(description + " off");
	}
 
	public void setFrequency(double frequency) {
		System.out.println(description + " setting frequency to " + frequency);
		this.frequency = frequency;
	}
 
	public void setAm() {
		System.out.println(description + " setting AM mode");
	}
 
	public void setFm() {
		System.out.println(description + " setting FM mode");
	}
  
        public String toString() {
                return description;
        }
}
