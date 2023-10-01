import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        List<Celebritie> celebsList = makeList();
        Celebritie[] celebsArray = makeArray();

        // Using list
        listExample(celebsList);

        // using array
        arrayExample(celebsArray);
    }

    public static void listExample(List<Celebritie> celebsList) {
        List<Celebritie> songwriters = celebsList.stream().filter(c -> "Songwriting".equals(c.getCourse())).toList();
        List<String> songwritersRepresentation = songwriters.stream().map(Celebritie::toString).toList();
        List<Double> songwritersMoney = songwriters.stream().map(Celebritie::getMoney).toList();
        Double sumSongwritersMoney = songwritersMoney.stream().reduce(Double::sum).orElse(0.0);

        Double sumSongwritersMoneyOneLine = songwriters.stream()
                .filter(c -> "Songwriting".equals(c.getCourse()))
                .map(Celebritie::getMoney)
                .reduce(Double::sum)
                .orElse(0.0);
    }

    public static void arrayExample(Celebritie[] celebsArray) {
        Celebritie[] songwriters = Arrays.stream(celebsArray).filter(c -> "Songwriting".equals(c.getCourse())).toArray(Celebritie[]::new);
        String[] songwritersRepresentation = Arrays.stream(songwriters).map(Celebritie::toString).toArray(String[]::new);
        Double[] songwritersMoney = Arrays.stream(songwriters).map(Celebritie::getMoney).toArray(Double[]::new);
        Double sumSongwritersMoney = Arrays.stream(songwritersMoney).reduce(Double::sum).orElse(0.0);

        Double sumSongwritersMoneyOneLine = Arrays.stream(celebsArray)
                .filter(c -> "Songwriting".equals(c.getCourse()))
                .map(Celebritie::getMoney)
                .reduce(Double::sum)
                .orElse(0.0);
    }

    public static List<Celebritie> makeList() {
        List<Celebritie> list = new ArrayList<>();

        list.add(new Celebritie("Taylor Swift", "Songwriting", Double.POSITIVE_INFINITY, 30000000.0));
        list.add(new Celebritie("Robert Downey Jr.", "Acting", 10.0, 20000000.0));
        list.add(new Celebritie("Jake Gyllenhaal", "Acting", 4.0, 1000.0));
        list.add(new Celebritie("Olivia Rodrigo", "Songwriting", 8.0, 100000.0));
        list.add(new Celebritie("Bruno Mars", "Songwriting", 10.0, 10000000.0));
        list.add(new Celebritie("Shawn Mendes", "Songwriting", 8.0, 1000000.0));
        list.add(new Celebritie("Travis Kelce", "Football player", 10.0, 1000000.0));
        list.add(new Celebritie("Joe Burrow", "Football player", 10.0, 1000000.0));

        return list;
    }

    public static Celebritie[] makeArray() {
        Celebritie[] array = new Celebritie[8];

        array[0] = new Celebritie("Taylor Swift", "Songwriting", Double.POSITIVE_INFINITY, 30000000.0);
        array[1] = new Celebritie("Robert Downey Jr.", "Acting", 10.0, 20000000.0);
        array[2] = new Celebritie("Jake Gyllenhaal", "Acting", 4.0, 1000.0);
        array[3] = new Celebritie("Olivia Rodrigo", "Songwriting", 8.0, 100000.0);
        array[4] = new Celebritie("Bruno Mars", "Songwriting", 10.0, 10000000.0);
        array[5] = new Celebritie("Shawn Mendes", "Songwriting", 8.0, 1000000.0);
        array[6] = new Celebritie("Travis Kelce", "Football player", 10.0, 1000000.0);
        array[7] = new Celebritie("Joe Burrow", "Football player", 10.0, 1000000.0);

        return array;
    }
}