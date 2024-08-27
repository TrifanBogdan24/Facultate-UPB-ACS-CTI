package fileio.input;

import lombok.Data;

@Data
public final class UserInput {
    private String username;
    private int age;
    private String city;

    public UserInput() {
    }


    @Override
    public String toString() {
        return "UserInput{"
                + "username='" + username + '\''
                + ", age=" + age
                + ", city='" + city + '\''
                + '}';
    }
}
