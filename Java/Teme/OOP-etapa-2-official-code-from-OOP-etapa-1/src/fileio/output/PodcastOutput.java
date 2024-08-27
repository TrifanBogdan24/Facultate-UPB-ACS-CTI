package fileio.output;

import lombok.Data;

import java.util.ArrayList;

@Data
public class PodcastOutput {

    private String name;
    private ArrayList<String> episodes;

    public PodcastOutput() {

    }

    public PodcastOutput(final String name, final ArrayList<String> episodes) {
        this.name = name;
        this.episodes = episodes;
    }
}
