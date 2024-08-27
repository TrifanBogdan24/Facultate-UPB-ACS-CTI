package main;

import fileio.input.PodcastInput;
import fileio.input.SongInput;
import lombok.Data;

import java.util.ArrayList;

@Data
public final class SingletonInformation {

    // SEACH :
    private ArrayList<String> resultedNamesForSearch;
    private ArrayList<SongInput> resultedSearchedSongs;
    private ArrayList<PodcastInput> resultedSearchedPodcasts;
    private ArrayList<Playlist> resultedSearchedPlaylists;


    // SELECT :
    private String resultSelect;
    private SongInput selectedSong;
    private PodcastInput selectedPodcast;
    private Playlist selectedPlaylist;



    // LOAD :

    // redare (load) song
    private SongInput loadedSong;
    private int currentSongTime;
    private int remainedSongTime;
    private int repeatSongCounter;
    private int prevSongTimestamp;


    // redare (load) playlist
    private Playlist loadedPlaylist;
    private int idxCurrentPlaylistSong;       // melodia curenta
    private int timeCurrentPlaylistSong;      // timpul curent
    private int remainedTimeCurrentPlaylistSong;
    private int repeatPlaylistSongCounter;
    private int prevPlaylistTimestamp;


    // redare (load) podcast
    private PodcastInput loadedPodcast;
    private int idxCurrentPodcastEpisode;
    private int timeCurrentPodcastEpisode;
    private int counterPodcastRepeat;
    private int prevPodcastTimestamp;
    private int remainedEpisodeTime;



    // PLAY PAUSE :
    private String sourceName;


    // STATUS :
    // informatiile date comenzii de iesire
    private String nameSourceToLoad;          // poate fi nume de melodie, playlist sau podcast
    private int repeatType;
    private boolean paused;
    private boolean shuffle;


    // informatii despre trecutul player-ului
    private boolean loaded;


    private static SingletonInformation instance = new SingletonInformation();

    /** ascundem constructorul facandu-l private  */
    public static SingletonInformation getInstance() {
        // se va intoarce unica instanta sau null
        return instance;
    }


    private SingletonInformation() {

    }


    /**
     * initializeaza player-ul
     */
    public void initFields() {
        // SEARCH :
        this.resultedNamesForSearch = null;
        this.resultedSearchedSongs = null;
        this.resultedSearchedPodcasts = null;
        this.resultedSearchedPlaylists = null;

        // SELECT :
        this.resultSelect = null;
        this.selectedSong = null;
        this.selectedPlaylist = null;

        // LOAD :

        // redare (load) song :
        this.loadedSong = null;
        this.currentSongTime = 0;
        this.remainedSongTime = 0;
        this.repeatSongCounter = 0;
        this.prevSongTimestamp = 0;

        // redare (load) playlist :
        this.loadedPlaylist = null;
        this.idxCurrentPlaylistSong = 0;
        this.timeCurrentPlaylistSong = 0;
        this.remainedTimeCurrentPlaylistSong = 0;
        this.repeatPlaylistSongCounter = 0;
        this.prevPlaylistTimestamp = 0;

        // redare (load) podcast
        this.loadedPodcast = null;
        this.idxCurrentPodcastEpisode = 0;
        this.timeCurrentPodcastEpisode = 0;
        this.counterPodcastRepeat = 0;
        this.prevPodcastTimestamp = 0;
        this.remainedEpisodeTime = 0;

        // PLAY PAUSE
        this.sourceName = "";

        // STATUS
        this.nameSourceToLoad = "";
        this.repeatType = 0;        // No Repeat
        this.paused = true;
        this.shuffle = false;
    }

    /**
     * functie vida fara parametri care face sa dispara si anuleaza player-ul curent
     * anuleaza informatiile depsre melodia / playlist-ul / podcast-ul redat
     */
    public void unloadPlayer() {

        this.loadedSong = null;
        this.loadedPlaylist = null;
        this.loadedPodcast = null;

        this.loaded = false;

        this.nameSourceToLoad = "";
        this.shuffle = false;
        this.paused = true;
    }



    /**
     * atunci cand trece mai mult timp decat durata melodiei
     * luand in considerare si tipul de repeat
     * va trebui sa anulam playlist-ul redat
     * sursa de load remanand neocupata
     */
    public void anulateSongPlayer() {
        // nu se mai ruleaza nimic (nici nu se va ma repeta nimic)

        this.remainedEpisodeTime = 0;
        this.currentSongTime = 0;

        this.unloadPlayer();
    }



    /**
     * atunci cand trece mai mult timp decat durata podcast-ului
     * luand in considerare si tipul de repeat
     * va trebui sa anulam podcast-ul redat
     * sursa de load remanand neocupata
     */
    public void podcastPlayerOutOfBounds() {
        // nu se mai ruleaza nimic (nici nu se va repeta nimic)

        this.loadedPodcast = null;
        this.idxCurrentPodcastEpisode = 0;
        this.timeCurrentPodcastEpisode = 0;
        this.remainedEpisodeTime = 0;

        this.unloadPlayer();
    }



    /**
     * atunci cand trece mai mult timp decat durata playlist-ului
     * luand in considerare si tipul de repeat
     * va trebui sa anulam playlist-ul redat
     * sursa de load remanand neocupata
     */
    public void playlistPlayerOutOfBounds() {
        // nu se mai ruleaza nimic

        this.loadedPlaylist = null;
        this.idxCurrentPlaylistSong = 0;
        this.timeCurrentPlaylistSong = 0;

        this.unloadPlayer();
    }

    /**
     *
     * @return -> returneaza String-ul care va fi pus in field-ul pentru comanda de STATUS
     *              outcmd.stats.repeat
     */
    public String repeatForStatsField() {

        int repeat = this.getRepeatType();

        if (repeat == 0 && this.getLoadedPlaylist() != null) {
            return "No Repeat";
        } else if (repeat == 1 && this.getLoadedPlaylist() != null) {
            return "Repeat All";
        } else if (repeat == 2 && this.getLoadedPlaylist() != null) {
            return "Repeat Current Song";
        } else if (repeat == 0 && this.getLoadedSong() != null) {
            return "No Repeat";
        } else if (repeat == 1 && this.getLoadedSong() != null) {
            return "Repeat Once";
        } else if (repeat == 2 && this.getLoadedSong() != null) {
            return "Repeat Infinite";
        } else if (repeat == 0 && this.getLoadedPodcast() != null) {
            return "No Repeat";
        } else if (repeat == 1 && this.getLoadedPodcast() != null) {
            return  "Repeat Once";
        } else if (repeat == 2 && this.getLoadedPodcast() != null) {
            return "Repeat Infinite";
        }

        return "No Repeat";
    }

    /**
     *
     * @return -> returneaza String-ul pentru mesajul comenzii REPEAT
     */
    public String repeatCmdMessage() {

        int repeat = this.getRepeatType();
        String msg = "Repeat mode changed to ";

        if (repeat == 0 && this.getLoadedPlaylist() != null) {
            return (msg + "no repeat.");
        } else if (repeat == 1 && this.getLoadedPlaylist() != null) {
            return (msg + "repeat all.");
        } else if (repeat == 2 && this.getLoadedPlaylist() != null) {
            return (msg + "repeat current song.");
        } else if (repeat == 0 && this.getLoadedSong() != null) {
            return (msg + "no repeat.");
        } else if (repeat == 1 && this.getLoadedSong() != null) {
            return (msg + "repeat once.");
        } else if (repeat == 2 && this.getLoadedSong() != null) {
            return (msg + "repeat infinite.");
        } else if (repeat == 0 && this.getLoadedPodcast() != null) {
            return (msg + "no repeat.");
        } else if (repeat == 1 && this.getLoadedPodcast() != null) {
            return  (msg + "repeat once.");
        } else if (repeat == 2 && this.getLoadedPodcast() != null) {
            return (msg + "repeat infinite.");
        }

        return (msg + "no repeat.");
    }
}
