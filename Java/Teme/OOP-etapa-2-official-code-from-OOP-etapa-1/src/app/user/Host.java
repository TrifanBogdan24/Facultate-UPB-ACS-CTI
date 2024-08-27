package app.user;

import app.Announcement;
import app.audio.Collections.Podcast;
import app.player.Player;
import app.searchBar.SearchBar;
import app.utils.Enums;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.ArrayList;

@EqualsAndHashCode(callSuper = true)
@Data
public class Host extends User {

    private ArrayList<Podcast> podcasts;
    private ArrayList<Announcement> announcements;

    public Host() {
        super.setPageType(Enums.PageType.HOST);
        this.podcasts = new ArrayList<>();
        this.announcements = new ArrayList<>();
    }

    /**
     * Instantiates a new Artist (which is subclass of User)
     *
     * @param username the username
     * @param age      the age
     * @param city     the city
     */
    public Host(final String username, final int age, final String city) {
        super.setUsername(username);
        super.setAge(age);
        super.setCity(city);
        super.setPlaylists(new ArrayList<>());
        super.setLikedSongs(new ArrayList<>());
        super.setFollowedPlaylists(new ArrayList<>());
        super.setPlayer(new Player());
        super.setSearchBar(new SearchBar(username));
        super.setLastSearched(false);

        super.setPageType(Enums.PageType.HOST);
        this.podcasts = new ArrayList<>();
        this.announcements = new ArrayList<>();
    }


    /**
     * Gets a podcast
     *
     * @param name  the podcast name
     * @return      the podcast
     */
    public Podcast getHostPodcastByName(final String name) {
        for (Podcast podcast: this.podcasts) {
            if (podcast.getName().equals(name)) {
                return podcast;
            }
        }

        return null;
    }

    /**
     * Gets an announcement
     *
     * @param name  the announcement name
     * @return      the announcement
     */
    public Announcement getHostAnnouncementByName(final String name) {
        for (Announcement announcement: this.announcements) {
            if (announcement.getName().equals(name)) {
                return announcement;
            }
        }

        return null;
    }
    /**
     * Gets all Hosts's episodes
     *
     * @return      all episodes
     */
    public ArrayList<String> getAllEpisodesNames() {
        ArrayList<String> names = new ArrayList<>();

        for (Podcast podcast: this.podcasts) {
            names.addAll(podcast.getEpisodesNames());

        }
        return names;
    }
}
