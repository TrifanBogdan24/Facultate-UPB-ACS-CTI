package main.stats;

import fileio.input.EpisodeInput;
import fileio.input.PodcastInput;
import fileio.input.SongInput;
import fileio.input.LibraryInput;

import main.FunctionHeader;
import main.Playlist;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

public final class Status implements FunctionHeader {


    private Status() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/

     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Status obj = new Status();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * status
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                                  final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (info.getLoadedSong() != null) {
            SongStatus.showSongStatus(library, inputcmd, outputcmd);
        } else if (info.getLoadedPlaylist() != null) {
            PlaylistStatus.showPlaylistStatus(library, inputcmd, outputcmd);
        } else if (info.getLoadedPodcast() != null) {
            PodcastStatus.showPodcastStatus(inputcmd);
        }

        setStatusForOutput(library, outputcmd);
    }

    /**
     * updateaza (timpul) unde am ramas cu melodia / playlist-ul / podcast-ul incarcat
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     *
     * @param outputcmd modifica comanda de iesire, care va afisata din fisierele din result/
     */
    public static void setStatusForOutput(final LibraryInput library, final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();
        OutCmd.Stat stat = new OutCmd.Stat();

        if (info.getLoadedSong() != null) {

            String songName = info.getLoadedSong().getName();
            stat.setRemainedTime(info.getRemainedSongTime());
            stat.setName(songName);

        } else if (info.getLoadedPlaylist() != null) {

            int idxPlaylistSong = info.getIdxCurrentPlaylistSong();
            int timePlaylistSong = info.getTimeCurrentPlaylistSong();

            Playlist playlist = info.getLoadedPlaylist();
            SongInput song = playlist.getSongs().get(idxPlaylistSong);

            stat.setName(song.getName());

            // timpul ramas din melodia curenta
            stat.setRemainedTime(song.getDuration() - timePlaylistSong);

        } else if (info.getLoadedPodcast() != null) {

            int idxEpisod = info.getIdxCurrentPodcastEpisode();
            int timeEpisode = info.getTimeCurrentPodcastEpisode();

            PodcastInput podcast = info.getLoadedPodcast();
            EpisodeInput episod = podcast.getEpisodes().get(idxEpisod);

            stat.setName(episod.getName());

            // timpul ramas din episodul curent

            stat.setRemainedTime(info.getRemainedEpisodeTime());

        } else {
            // player-ul nu reda nimic la momentul de timp actual
            stat.setName("");
        }


        stat.setRepeat(info.repeatForStatsField());
        stat.setShuffle(info.isShuffle());
        stat.setPaused(info.isPaused());
        outputcmd.setStats(stat);
    }

}
