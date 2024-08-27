package commands.hostcmds;

import app.SingletonAdmin;
import app.audio.Collections.AudioCollection;
import app.audio.Collections.Podcast;
import app.user.Host;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;

public class RemovePodcast implements CommandRunner {
    public RemovePodcast() {
    }

    /**
     * RemovePodcast object node
     *
     * @param commandInput the input command from the input/test* files
     * @return the ouput command, on object node that will be written
     * in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        String message = removePodcastMessage(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }

    private static String removePodcastMessage(final CommandInput commandInput) {
        String username = commandInput.getUsername();
        String podcastName = commandInput.getName();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);

        if (user == null) {
            return ("The username " + username + " doesn't exist.");
        }

        // removing the loaded audio file from the player
        user.anulatesUserLoad();
        user.setTimestampLastPlayedFile(null);

        if (!user.getClass().equals(Host.class)) {
            return (username + " is not a host.");
        }

        Host host = (Host) user;
        Podcast removedPodcast = host.getHostPodcastByName(podcastName);
        admin.simulateTimeForAllUsers(commandInput.getTimestamp());

        if (removedPodcast == null) {
            return (username + " doesn't have a podcast with the given name.");
        }

        if (isPodcastInLoad(removedPodcast, user)) {
            return (username + " can't delete this podcast.");
        }

        // podcast can be successfully deleted

        // deleting podcast from host collection
        ArrayList<Podcast> hostPodcast = host.getPodcasts();
        hostPodcast.remove(removedPodcast);
        host.setPodcasts(hostPodcast);

        // deleting podcast from the database
        ArrayList<Podcast> adminPodcasts = admin.getPodcasts();
        adminPodcasts.remove(removedPodcast);
        admin.setPodcasts(adminPodcasts);

        return (username + " deleted the podcast successfully.");
    }

    public static boolean isPodcastInLoad(final Podcast podcast, final User userWhoRemoves) {
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

            if (!source.getClass().equals(Podcast.class)) {
                continue;
            }

            Podcast pod = (Podcast) source;

            if (pod != podcast) {
                continue;
            }

            if (pod.getOwner().equals(userWhoRemoves.getUsername())) {
                return true;
            }
        }

        return false;
    }
}
