package app.user;

import app.Event;
import app.Merch;
import app.audio.Collections.Album;
import app.player.Player;
import app.searchBar.SearchBar;
import app.utils.Enums;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.ArrayList;

@EqualsAndHashCode(callSuper = true)
@Data
public class Artist extends User {
    private ArrayList<Event> events;
    private ArrayList<Merch> merchs;
    private ArrayList<Album> albums;

    public Artist() {
        super.setPageType(Enums.PageType.ARTIST);
        this.events = new ArrayList<>();
        this.merchs = new ArrayList<>();
        this.albums = new ArrayList<>();
    }

    /**
     * Instantiates a new Artist (which is subclass of User)
     *
     * @param username the username
     * @param age      the age
     * @param city     the city
     */
    public Artist(final String username, final int age, final String city) {
        super.setPageType(Enums.PageType.ARTIST);
        super.setUsername(username);
        super.setAge(age);
        super.setCity(city);
        super.setPlaylists(new ArrayList<>());
        super.setLikedSongs(new ArrayList<>());
        super.setFollowedPlaylists(new ArrayList<>());
        super.setPlayer(new Player());
        super.setSearchBar(new SearchBar(username));
        super.setLastSearched(false);
        this.events = new ArrayList<>();
        this.merchs = new ArrayList<>();
        this.albums = new ArrayList<>();
    }

    /**
     *
     * @param name      the name of an Event
     * @return          the Event that has the same name as the parameter
     */
    public Event getEventByName(final String name) {

        for (Event event: this.getEvents()) {
            if (event != null && event.getName().equals(name)) {
                return event;
            }
        }

        return null;
    }

    /**
     *
     * @param name      the name of an Album
     * @return          the Album that has the same name as the parameter
     */
    public Album getAlbumByName(final String name) {

        for (Album album: this.getAlbums()) {
            if (album != null && album.getName().equals(name)) {
                return album;
            }
        }
        return null;
    }

    /**
     *
     * @param name      the name of a Merch
     * @return          the Merch that has the same name as the parameter
     */
    public Merch getMerchByName(final String name) {

        for (Merch merch: this.getMerchs()) {
            if (merch != null && merch.getName().equals(name)) {
                return merch;
            }
        }

        return null;
    }
}
