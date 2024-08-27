package app;

import lombok.Data;

@Data
public class Announcement {

    private String name;
    private String description;
    public Announcement() {
    }

    /**
     *
     * @param name          the name of the announcement
     * @param description   the description of the announcement
     */
    public Announcement(final String name, final String description) {
        this.name = name;
        this.description = description;
    }
}
