package fileio.input;

import lombok.Data;

@Data
public final class EpisodeInput {
    private String name;
    private Integer duration;
    private String description;

    public EpisodeInput() {
    }


    /**
     *
     * @param name          the name of the input/ouput episode
     * @param duration      the duration of the input/ouput episode
     * @param description   the description of the input/ouput episode
     */
    public EpisodeInput(final String name, final Integer duration, final String description) {
        this.name = name;
        this.duration = duration;
        this.description = description;
    }

    @Override
    public String toString() {
        return "EpisodeInput{"
                + "name='" + name + '\''
                + ", description='" + description + '\''
                + ", duration=" + duration
                + '}';
    }
}
