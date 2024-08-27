package fileio.input;

import java.util.ArrayList;
import lombok.Data;


@Data
public final class PodcastInput {
    private String name;
    private String owner;
    private ArrayList<EpisodeInput> episodes;


    public PodcastInput() {
    }


}
