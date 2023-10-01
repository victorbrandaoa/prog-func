public class Celebritie {

    private String name;

    private String course;

    private Double grade;

    private Double money;

    public Celebritie(String name, String course, Double grade, Double money) {
        this.name = name;
        this.course = course;
        this.grade = grade;
        this.money = money;
    }

    public String getName() {
        return this.name;
    }

    public String getCourse() {
        return this.course;
    }

    public Double getGrade() {
        return this.grade;
    }

    public Double getMoney() {
        return this.money;
    }

    @Override
    public String toString() {
        return String.format("Name: %s; Course: %s; Grade: %.1f; Money: %.1f", this.name, this.course, this.grade, this.money);
    }
}
