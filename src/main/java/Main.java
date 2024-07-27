public class Main {
    public static void main(String[] args) {

        Singleton s = Singleton.getInstance();

    }

    abstract class Animal {
        abstract String youAre();
    }

    class Tiger extends Animal {
        
        String iAm;
        @Override
        public String youAre() {
            return iAm;
        }
    }

    class Lion extends Animal {
        String iAm;
        @Override
        public String youAre() {
            return iAm;
        }
    }

    class Rabbit extends Animal {
        String iAm;
        @Override
        public String youAre() {
            return iAm;
        }
    }

    public void animalType(Animal animal) {
        if (animal instanceof Rabbit) {
            Rabbit rab = (Rabbit) animal;
            System.out.println(rab.iAm + " is Herbivore");
        } else if (animal instanceof Lion || animal instanceof Tiger) {
            // I have to do to more if-else
            System.out.println("Carnivore");
        }

    }


}
