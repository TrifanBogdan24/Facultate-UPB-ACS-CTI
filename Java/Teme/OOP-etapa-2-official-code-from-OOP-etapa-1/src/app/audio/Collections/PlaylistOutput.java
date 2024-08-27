package app.audio.Collections;

import app.audio.Playlist;
import app.utils.Enums;
import lombok.Data;

import java.util.ArrayList;

@Data
public class PlaylistOutput {
    private final String name;
    private final ArrayList<String> songs;
    private final String visibility;
    private final int followers;


    /**
     *
     * @param playlist      all public playlists of all users
     *                      they will be leter put in and displayed in
     *                      the output commands
     */
    public PlaylistOutput(final Playlist playlist) {
        this.name = playlist.getName();
        this.songs = new ArrayList<>();
        for (int i = 0; i < playlist.getSongs().size(); i++) {
            songs.add(playlist.getSongs().get(i).getName());
        }
        this.visibility = playlist.getVisibility() == Enums.Visibility.PRIVATE
                ? "private" : "public";
        this.followers = playlist.getFollowers();
    }

}
