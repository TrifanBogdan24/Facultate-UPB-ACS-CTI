package main.stats;


import fileio.input.LibraryInput;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

public final class SongStatus {

    private SongStatus() {

    }

    /**
     * metoda verifica si updeteaza starea melodiei din loader
     * in functie de care se vor seta field-uri din clasa 'SingletonInformation'
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public static void showSongStatus(final LibraryInput library, final InCmd inputcmd,
                                      final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();
        OutCmd.Stat stat = new OutCmd.Stat();

        // rulam o melodie

        if (!info.isPaused()) {
            updateSongPlayTime(inputcmd.getTimestamp());
            info.setPrevSongTimestamp(inputcmd.getTimestamp());
        }

        stat.setRepeat(info.repeatForStatsField());
        stat.setRemainedTime(info.getRemainedSongTime());

        if (info.getLoadedSong() == null) {
            stat.setName("");
        } else {
            stat.setName(info.getLoadedSong().getName());
        }

        stat.setPaused(info.isPaused());
        outputcmd.setStats(stat);
    }

    /**
     * updatam durata ramasa din melodia curenta,
     * si implicit si durata (timpul) redata din aceasta
     *
     * @param currentTimestamp timpul curent
     */
    public static void updateSongPlayTime(final int currentTimestamp) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (info.getLoadedSong() == null) {
            return;
        }

        // diferenta intre timestamp-urile comenzilor
        int timeDiff = currentTimestamp - info.getPrevSongTimestamp();

        int songTime = info.getCurrentSongTime();
        int songDuration = info.getLoadedSong().getDuration();

        songTime += timeDiff;

        if (songTime < songDuration) {
            setSongTime(songTime, songDuration);
            return;
        }

        // now : songTime >= songDuration

        if (info.getRepeatType() == 2) {
            // repeat infinite
            while (songTime >= songDuration) {
                songTime -= songDuration;
            }
            setSongTime(songTime, songDuration);
            return;
        }

        if (info.getRepeatType() == 1) {
            // repeat once -> trece o melodie -> no repeat

            info.setRepeatType(0);
            songTime -= songDuration;

            if (songTime >= songDuration) {
                info.anulateSongPlayer();
                return;
            } else {
                setSongTime(songTime, songDuration);
                return;
            }
        }

        info.anulateSongPlayer();
    }

    /**
     * updateaza timpul de redare al melodiei curente
     *
     * @param songTime -> timpul scurs din melodia curenta
     * @param songDuration -> durata melodiei curente
     */
    public static void setSongTime(final int songTime, final int songDuration) {
        SingletonInformation info = SingletonInformation.getInstance();
        info.setCurrentSongTime(songTime);
        info.setRemainedSongTime(songDuration - songTime);
    }



}
