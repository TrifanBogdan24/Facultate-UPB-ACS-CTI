package fileio.input;

import lombok.Data;

import java.util.ArrayList;

@Data
public final class Input {
    private LibraryInput library;
    private ArrayList<UserInput> users;
    private ArrayList<CommandInput> commands;

    public Input() {
    }

    @Override
    public String toString() {
        return "AppInput{"
                + "library=" + library
                + ", users=" + users
                + ", commands=" + commands
                + '}';
    }
}
