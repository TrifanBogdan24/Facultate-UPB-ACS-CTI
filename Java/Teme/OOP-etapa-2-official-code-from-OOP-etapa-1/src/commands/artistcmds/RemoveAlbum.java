package commands.artistcmds;

import app.SingletonAdmin;
import app.audio.Collections.Album;
import app.audio.Collections.AudioCollection;
import app.audio.Collections.Podcast;
import app.audio.Files.Song;
import app.user.Artist;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;

public final class RemoveAlbum implements CommandRunner {
    public RemoveAlbum() {
    }

    /**
     * RemoveAlbum object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        String message = removeAlbumMessage(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }

    /**
     * RemoveAlbum message
     *
     * @param commandInput       from the input/test* files
     * @return                   the message generated by the execution of the input command
     */
    private static String removeAlbumMessage(final CommandInput commandInput) {
        String username = commandInput.getUsername();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);

        if (user == null) {
            return ("The username " + username + " doesn't exist.");
        }

        // removing the loaded audio file from the player
        user.anulatesUserLoad();
        user.setTimestampLastPlayedFile(null);

        if (!user.getClass().equals(Artist.class)) {
            return (username + " is not an artist.");
        }

        Artist artist = (Artist) user;      // down-casting from User to Artist
        String name = commandInput.getName();
        Album removedAlbum = artist.getAlbumByName(name);
        admin.simulateTimeForAllUsers(commandInput.getTimestamp());

        if (removedAlbum == null) {
            return (username + " doesn't have an album with the given name.");
        }

        if (isAlbumInLoad(removedAlbum, user)) {
            // there is at least one user who is loading this album
            return (username + " can't delete this album.");
        }

        // the album can be deleted successfully

        // removing album from artist collection
        ArrayList<Album> artistAlbums = artist.getAlbums();
        artistAlbums.remove(removedAlbum);
        artist.setAlbums(artistAlbums);

        // deleting album from the database
        ArrayList<Album> adminAlbums = admin.getAlbums();
        adminAlbums.remove(removedAlbum);
        admin.setAlbums(adminAlbums);

        // updating the songs in the database
        // the songs from the album must be deleted from the database
        ArrayList<Song> adminSongs = admin.getSongs();
        adminSongs.removeAll(removedAlbum.getSongs());
        admin.setSongs(adminSongs);

        return (username + " deleted the album successfully.");
    }


    public static boolean isAlbumInLoad(final Album album, final User userWhoRemoves) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        for (User user: admin.getUsers()) {
            if (user == userWhoRemoves) {
                continue;
            }

            if (user.getPlayer() == null
                    || user.getPlayer().getSource() == null
                    || user.getPlayer().getSource().getAudioCollection() == null) {
                continue;
            }

            AudioCollection source = user.getPlayer().getSource().getAudioCollection();

            if (!source.getClass().equals(Album.class)) {
                continue;
            }

            Album ab = (Album) source;

            if (ab != album) {
                continue;
            }

            if (ab.getOwner().equals(userWhoRemoves.getUsername())) {
                return true;
            }
        }

        return false;
    }
}
