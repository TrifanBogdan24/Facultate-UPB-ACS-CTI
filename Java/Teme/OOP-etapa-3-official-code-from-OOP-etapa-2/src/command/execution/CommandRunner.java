package command.execution;

import app.SingletonAdmin;
import fileio.input.CommandInput;

import command.execution.factory.commands.FactoryAddAlbum;
import command.execution.factory.commands.FactoryAddAnnouncement;
import command.execution.factory.commands.FactoryAddEvent;
import command.execution.factory.commands.FactoryAddMerch;
import command.execution.factory.commands.FactoryAddPodcast;
import command.execution.factory.commands.FactoryAddRemoveInPlaylist;
import command.execution.factory.commands.FactoryAddUser;
import command.execution.factory.commands.FactoryBackward;
import command.execution.factory.commands.FactoryChangePage;
import command.execution.factory.commands.FactoryCreatePlaylist;
import command.execution.factory.commands.FactoryDeleteUser;
import command.execution.factory.commands.FactoryFollow;
import command.execution.factory.commands.FactoryForward;
import command.execution.factory.commands.FactoryGetAllUsers;
import command.execution.factory.commands.FactoryGetOnlineUsers;
import command.execution.factory.commands.FactoryGetPreferredGenre;
import command.execution.factory.commands.FactoryGetTop5Albums;
import command.execution.factory.commands.FactoryGetTop5ArtistsList;
import command.execution.factory.commands.FactoryGetTop5Playlists;
import command.execution.factory.commands.FactoryGetTop5Songs;
import command.execution.factory.commands.FactoryLike;
import command.execution.factory.commands.FactoryLoad;
import command.execution.factory.commands.FactoryNext;
import command.execution.factory.commands.FactoryPlayPause;
import command.execution.factory.commands.FactoryPrev;
import command.execution.factory.commands.FactoryPrintCurrentPage;
import command.execution.factory.commands.FactoryRemoveAlbum;
import command.execution.factory.commands.FactoryRemoveAnnouncement;
import command.execution.factory.commands.FactoryRemoveEvent;
import command.execution.factory.commands.FactoryRemovePodcast;
import command.execution.factory.commands.FactoryRepeat;
import command.execution.factory.commands.FactorySearch;
import command.execution.factory.commands.FactorySelect;
import command.execution.factory.commands.FactoryShowAlbums;
import command.execution.factory.commands.FactoryShowPlaylists;
import command.execution.factory.commands.FactoryShowPodcasts;
import command.execution.factory.commands.FactoryShowPreferredSongs;
import command.execution.factory.commands.FactoryShuffle;
import command.execution.factory.commands.FactoryStatus;
import command.execution.factory.commands.FactorySwitchConnectionStatus;
import command.execution.factory.commands.FactorySwitchVisibility;




// this class is Singleton Factory
public final class CommandRunner {
    private static CommandRunner instance = null;

    private CommandRunner() {

    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static CommandRunner getInstance() {
        if (instance == null) {
            instance = new CommandRunner();
        }
        return instance;
    }


    /**
     * Update admin.
     */
    public static void updateAdmin() {
        SingletonAdmin admin = SingletonAdmin.getInstance();
    }

    /**
     *
     * @param commandInput  the input command
     * @return              an instance of a class that implements Command interface
     */
    public Command createCommand(final CommandInput commandInput) {

        FactoryCommand factoryCmd = null;


        switch (commandInput.getCommand()) {
            case "search":
                factoryCmd = new FactorySearch();
                break;
            case "select":
                factoryCmd = new FactorySelect();
                break;
            case "load":
                factoryCmd = new FactoryLoad();
                break;
            case "playPause":
                factoryCmd = new FactoryPlayPause();
                break;
            case "repeat":
                factoryCmd = new FactoryRepeat();
                break;
            case "shuffle":
                factoryCmd = new FactoryShuffle();
                break;
            case "forward":
                factoryCmd = new FactoryForward();
                break;
            case "backward":
                factoryCmd = new FactoryBackward();
                break;
            case "like":
                factoryCmd = new FactoryLike();
                break;
            case "next":
                factoryCmd = new FactoryNext();
                break;
            case "prev":
                factoryCmd = new FactoryPrev();
                break;
            case "createPlaylist":
                factoryCmd = new FactoryCreatePlaylist();
                break;
            case "addRemoveInPlaylist":
                factoryCmd = new FactoryAddRemoveInPlaylist();
                break;
            case "switchVisibility":
                factoryCmd = new FactorySwitchVisibility();
                break;
            case "showPlaylists":
                factoryCmd = new FactoryShowPlaylists();
                break;
            case "follow":
                factoryCmd = new FactoryFollow();
                break;
            case "status":
                factoryCmd = new FactoryStatus();
                break;
            case "showPreferredSongs":
                factoryCmd = new FactoryShowPreferredSongs();
                break;
            case "getPreferredGenre":
                factoryCmd = new FactoryGetPreferredGenre();
                break;
            case "switchConnectionStatus":
                factoryCmd = new FactorySwitchConnectionStatus();
                break;
            case "getOnlineUsers":
                factoryCmd = new FactoryGetOnlineUsers();
                break;
            case "addUser":
                factoryCmd = new FactoryAddUser();
                break;
            case "deleteUser":
                factoryCmd = new FactoryDeleteUser();
                break;
            case "getAllUsers":
                factoryCmd = new FactoryGetAllUsers();
                break;
            case "addAlbum":
                factoryCmd = new FactoryAddAlbum();
                break;
            case "removeAlbum":
                factoryCmd = new FactoryRemoveAlbum();
                break;
            case "addEvent":
                factoryCmd = new FactoryAddEvent();
                break;
            case "removeEvent":
                factoryCmd = new FactoryRemoveEvent();
                break;
            case "addMerch":
                factoryCmd = new FactoryAddMerch();
                break;
            case "addPodcast":
                factoryCmd = new FactoryAddPodcast();
                break;
            case "removePodcast":
                factoryCmd = new FactoryRemovePodcast();
                break;
            case "addAnnouncement":
                factoryCmd = new FactoryAddAnnouncement();
                break;
            case "removeAnnouncement":
                factoryCmd = new FactoryRemoveAnnouncement();
                break;
            case "showAlbums":
                factoryCmd = new FactoryShowAlbums();
                break;
            case "changePage":
                factoryCmd = new FactoryChangePage();
                break;
            case "printCurrentPage":
                factoryCmd = new FactoryPrintCurrentPage();
                break;
            case "showPodcasts":
                factoryCmd = new FactoryShowPodcasts();
                break;
            case "getTop5Songs":
                factoryCmd = new FactoryGetTop5Songs();
                break;
            case "getTop5Playlists":
                factoryCmd = new FactoryGetTop5Playlists();
                break;
            case "getTop5Albums":
                factoryCmd = new FactoryGetTop5Albums();
                break;
            case "getTop5Artists":
                factoryCmd = new FactoryGetTop5ArtistsList();
                break;
            default:
                return null;
        }

        return factoryCmd.createCommand(commandInput);
    }
}
