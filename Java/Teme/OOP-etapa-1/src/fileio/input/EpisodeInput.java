package fileio.input;
import lombok.Data;

@Data
public final class EpisodeInput {
    private String name;
    private Integer duration;
    private String description;

    public EpisodeInput() {
    }


}
