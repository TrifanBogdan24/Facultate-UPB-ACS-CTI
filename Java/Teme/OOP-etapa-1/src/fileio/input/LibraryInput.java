package fileio.input;

import java.util.ArrayList;
import lombok.Data;

@Data
public final class LibraryInput {
    private ArrayList<SongInput> songs;
    private ArrayList<PodcastInput> podcasts;
    private ArrayList<UserInput> users;

    public LibraryInput() {
    }

}
