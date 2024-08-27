package fileio.input;

import lombok.Data;

import java.util.ArrayList;

@Data
public final class PodcastInput {
    private String name;
    private String owner;
    private ArrayList<EpisodeInput> episodes;

    public PodcastInput() {
    }

    public PodcastInput(final String name, final String owner,
                        final ArrayList<EpisodeInput> episodes) {
        this.name = name;
        this.owner = owner;
        this.episodes = episodes;
    }

    @Override
    public String toString() {
        return "PodcastInput{"
                + "name='" + name + '\''
                + ", owner='" + owner + '\''
                + ", episodes=" + episodes
                + '}';
    }
}
