class Singleton {
    private static Singleton singleton = null;

    public String s;

    private Singleton() {
        s = "A Singleton Class";
    }

    public static synchronized Singleton getInstance() {
        if (singleton == null)
            singleton = new Singleton();

        return singleton;
    }
}