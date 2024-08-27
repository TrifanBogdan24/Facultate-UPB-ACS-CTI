package app;

import app.audio.Collections.Album;
import app.audio.Collections.AudioCollection;
import app.audio.Playlist;
import app.audio.Collections.Podcast;
import app.audio.Files.Episode;
import app.audio.Files.Song;
import app.user.Artist;
import app.user.Host;
import app.user.User;
import fileio.input.EpisodeInput;
import fileio.input.PodcastInput;
import fileio.input.SongInput;
import fileio.input.UserInput;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;


/**
 * The type SingletonAdmin.
 */
@Data
public final class SingletonAdmin {
    private ArrayList<User> users;
    private ArrayList<Song> songs;
    private ArrayList<Podcast> podcasts;

    private ArrayList<Event> events;
    private ArrayList<Album> albums;
    private ArrayList<Merch> merchs;
    private ArrayList<Announcement> announcements;

    private User currentUser;

    private static int timestamp = 0;

    private static final int LIMIT = 5;

    private static SingletonAdmin instance = null;

    /**
     * private contructor
     */
    private SingletonAdmin() {
    }

    /**
     *
     * @return  the unique instance of the class
     */

    public static SingletonAdmin getInstance() {
        if (instance == null) {
            instance = new SingletonAdmin();
        }
        return instance;
    }

    /**
     * Sets users.
     *
     * @param userInputList the user input list
     */
    public void setInputUsers(final List<UserInput> userInputList) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<User> usersInput = new ArrayList<>();
        for (UserInput userInput : userInputList) {
            usersInput.add(new User(userInput.getUsername(),
                    userInput.getAge(), userInput.getCity()));
        }

        admin.setUsers(usersInput);
    }

    /**
     * Sets songs.
     *
     * @param songInputList the song input list
     */
    public void setInputSongs(final List<SongInput> songInputList) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<Song> songsInput = new ArrayList<>();
        for (SongInput songInput : songInputList) {
            songsInput.add(new Song(songInput.getName(), songInput.getDuration(),
                    songInput.getAlbum(), songInput.getTags(), songInput.getLyrics(),
                    songInput.getGenre(), songInput.getReleaseYear(), songInput.getArtist()));
        }

        admin.setSongs(songsInput);
    }


    /**
     * Sets podcasts.
     *
     * @param podcastInputList the podcast input list
     */
    public void setInputPodcasts(final List<PodcastInput> podcastInputList) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        ArrayList podcastsInput = new ArrayList<>();

        for (PodcastInput podcastInput : podcastInputList) {
            List<Episode> episodes = new ArrayList<>();
            for (EpisodeInput episodeInput : podcastInput.getEpisodes()) {
                episodes.add(new Episode(episodeInput.getName(),
                                         episodeInput.getDuration(),
                                         episodeInput.getDescription()));
            }
            podcastsInput.add(new Podcast(podcastInput.getName(),
                    podcastInput.getOwner(), episodes));
        }

        admin.setPodcasts(podcastsInput);
    }

    /**
     *
     * @return      all playlists (of all users) stored in the database
     */
    public List<Playlist> getPlaylists() {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<Playlist> playlists = new ArrayList<>();
        for (User user : admin.getUsers()) {
            playlists.addAll(user.getPlaylists());
        }
        return playlists;
    }

    /**
     * Gets user.
     *
     * @param username the username
     * @return the user
     */
    public User getUserByName(final String username) {

        for (User user : users) {
            if (user.getUsername().equals(username)) {
                return user;
            }
        }
        return null;
    }


    /**
     * Gets an artist.
     *
     * @param artistname    the name of the artist
     * @return              the artist
     */
    public Artist getArtistByName(final String artistname) {

        for (Artist artist: this.getAllArtists()) {
            if (artist.getUsername().equals(artistname)) {
                return artist;
            }
        }

        return null;
    }

    /**
     *
     * @param hostname the name of the host
     * @return the host
     */
    public Host getHostByName(final String hostname) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        for (Host host: admin.getAllHosts()) {
            if (host.getUsername().equals(hostname)) {
                return host;
            }
        }

        return null;
    }
    /**
     *
     * @param name  the name of the song
     * @return      the Song having the exact name of the parameter
     */
    public Song getSongByName(final String name) {

        for (Song song: songs) {
            if (song.getName().equals(name)) {
                return song;
            }
        }
        return null;
    }

    /**
     *
     * @param name  the name of the merch
     * @return      the March having the exact name of the parameter
     */
    public Merch getMerchByName(final String name) {

        for (Merch merch: merchs) {
            if (merch.getName().equals(name)) {
                return merch;
            }
        }
        return null;
    }

    /**
     *
     * @param name  the name of the album
     * @return      the Album having the exact name of the parameter
     */
    public Album getAlbumByName(final String name) {
        for (Album album: albums) {
            if (album.getName().equals(name)) {
                return album;
            }
        }

        return null;
    }

    /**
     * Update timestamp.
     *
     * @param newTimestamp the new timestamp
     */
    public void updateTimestamp(final int newTimestamp) {

        int elapsed = newTimestamp - timestamp;
        timestamp = newTimestamp;
        if (elapsed == 0) {
            return;
        }

        for (User user : users) {
            user.simulateTime(elapsed);
        }
    }

    /**
     *
     * @return  formated Strings of songs for home page
     */
    public ArrayList<String> getSongsForHomePage() {
        ArrayList<String> names = new ArrayList<>();

        for (Song song: currentUser.getLikedSongs()) {
            names.add(song.getName());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of songs for liked content page
     */
    public ArrayList<String> getSongsForLikedContentPage() {
        ArrayList<String> names = new ArrayList<>();

        for (Song song: currentUser.getLikedSongs()) {
            names.add(song.getName() + " - " + song.getArtist());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of playlists for home page
     */
    public ArrayList<String> getPlaylistsForHomePage() {
        ArrayList<String> names = new ArrayList<>();

        for (Playlist playlist: currentUser.getFollowedPlaylists()) {
            names.add(playlist.getName());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of playlists for liked content page
     */
    public ArrayList<String> getPlaylistsForLikedPage() {
        ArrayList<String> names = new ArrayList<>();

        for (Playlist playlist: currentUser.getFollowedPlaylists()) {
            names.add(playlist.getName() + " - " + playlist.getOwner());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of albums for artist page
     */
    public ArrayList<String> getAlbumsForArtistPage() {
        ArrayList<String> names = new ArrayList<>();
        Artist artist = this.currentUser.getArtistForPage();


        for (Album album: artist.getAlbums()) {
            names.add(album.getName());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of merchs for artist page
     */
    public ArrayList<String> getMerchsForArtistPage() {
        ArrayList<String> names = new ArrayList<>();
        Artist artist = this.currentUser.getArtistForPage();

        for (Merch merch: artist.getMerchs()) {
            names.add(merch.getName() + " - " + merch.getPrice()
                    + ":\n\t" + merch.getDescription());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of events for artist page
     */
    public ArrayList<String> getEventsForArtisPAge() {
        ArrayList<String> names = new ArrayList<>();
        Artist artist = this.currentUser.getArtistForPage();

        for (Event event: artist.getEvents()) {
            names.add(event.getName() + " - " + event.getDate()
                    + ":\n\t" + event.getDecription());
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of podcasts for host page
     */
    public ArrayList<String> getPodcastsForHostPage() {
        ArrayList<String> names = new ArrayList<>();

        Host host = this.currentUser.getHostForPage();

        for (Podcast podcast: host.getPodcasts()) {
            names.add(podcast.getName() + ":\n\t" + podcast.getEpisodesForHostPage() + "\n");
        }

        return names;
    }

    /**
     *
     * @return  formated Strings of announcements for host page
     */

    public ArrayList<String> getAnnouncementsForHostPage() {
        ArrayList<String> names = new ArrayList<>();
        Host host = this.currentUser.getHostForPage();

        for (Announcement announcement: host.getAnnouncements()) {
            names.add(announcement.getName() + ":\n\t" + announcement.getDescription() + "\n");
        }

        return names;
    }



    /**
     *
     * @return  the names of all artists
     */
    public ArrayList<String> getAllArtistNames() {
        ArrayList<String> names = new ArrayList<>();

        for (User user: users) {
            if (user.getClass().equals(Artist.class)) {
                Artist artist = (Artist) user;
                names.add(artist.getUsername());
            }
        }

        return names;
    }

    /**
     *
     * @return      all users (that are not hosts / artists)
     */
    public ArrayList<User> getAllNormalUsers() {
        ArrayList<User> names = new ArrayList<>();

        for (User user: users) {
            if (user.getClass().equals(User.class)) {
                names.add(user);
            }
        }

        return names;
    }

    /**
     *
     * @return  all hosts (all users that are hosts)
     */
    public ArrayList<Host> getAllHosts() {
        ArrayList<Host> hosts = new ArrayList<>();

        for (User user: users) {
            if (user.getClass().equals(Host.class)) {
                Host host = (Host) user;
                hosts.add(host);
            }
        }

        return hosts;
    }


    /**
     *
     * @return  all artists (all users that are artists)
     */
    public ArrayList<Artist> getAllArtists() {
        ArrayList<Artist> artists = new ArrayList<>();

        for (User user: users) {
            if (user.getClass().equals(Artist.class)) {
                Artist artist = (Artist) user;
                artists.add(artist);
            }
        }

        return artists;
    }


    /**
     *
     * @param song  a song
     * @return      the user who owns the song
     */
    public User getOwnerAlbumSong(final Song song) {
        for (Album album: this.albums) {
            for (Song s: album.getSongs()) {
                if (song.getName().equals(s.getName())) {
                    String ownerName = album.getOwner();
                    return getUserByName(ownerName);
                }
            }
        }

        return null;
    }

    /**
     *
     * @param audioCollection   an audio file
     * @return                  the user who owns the audio file
     */
    public User getAudioCollectionOwner(final AudioCollection audioCollection) {
        for (User user: this.users) {
            if (audioCollection.getOwner().equals(user.getUsername())) {
                return user;
            }
        }
        return null;
    }


    /**
     *
     * @param currentTimestamp  the timestamp at which the current command is running
     */
    public void simulateTimeForAllUsers(final Integer currentTimestamp) {
        for (User user: this.users) {
            if (user.getTimestampLastPlayedFile() == null) {
                continue;
            }

            Integer lastTimestamp = user.getTimestampLastPlayedFile();
            user.simulateTime(currentTimestamp - lastTimestamp);
        }
    }


    /**
     * Reset.
     */
    public void reset() {
        users = new ArrayList<>();
        songs = new ArrayList<>();
        podcasts = new ArrayList<>();
        merchs = new ArrayList<>();
        events = new ArrayList<>();
        announcements = new ArrayList<>();
        albums = new ArrayList<>();
        timestamp = 0;
    }
}
