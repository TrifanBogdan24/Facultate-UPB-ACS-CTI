package commands.hostcmds;

import app.SingletonAdmin;
import app.audio.Collections.Podcast;
import app.audio.Files.Episode;
import app.user.Host;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;
import fileio.input.EpisodeInput;
import fileio.output.PodcastOutput;


import java.util.ArrayList;
import java.util.List;

public class ShowPodcasts implements CommandRunner {

    public ShowPodcasts() {

    }

    /**
     * AddAnnouncement object node
     *
     * @param commandInput the input command from the input/test* files
     * @return the ouput command, on object node that will be written
     * in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        ArrayList<PodcastOutput> result = showPodcastsResult(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(result));

        return objectNode;
    }

    /**
     *
     * @param commandInput      the input commands
     * @return                  the result field of the output command
     *                          all podcasts of the current host
     */
    private static ArrayList<PodcastOutput> showPodcastsResult(final CommandInput commandInput) {
        String username = commandInput.getUsername();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);

        // removing the loaded audio file from the player
        user.anulatesUserLoad();
        user.setTimestampLastPlayedFile(null);


        Host host = (Host) user;

        ArrayList<PodcastOutput> podcasts = new ArrayList<>();

        for (Podcast podcast : host.getPodcasts()) {
            String name = podcast.getName();
            String owner = podcast.getOwner();
            ArrayList<String> episodes = podcast.getEpisodesNames();
            podcasts.add(new PodcastOutput(name, episodes));
        }

        return podcasts;
    }

    /**
     *
     * @param episodes  an episode
     * @return          an episode for an ObjectNode that can be written in an output JSON file
     */
    public static ArrayList<EpisodeInput> convertEpisodesToInput(final List<Episode> episodes) {
        ArrayList<EpisodeInput> inputEpisodes = new ArrayList<>();

        for (Episode episode: episodes) {
            String name = episode.getName();
            Integer duration = episode.getDuration();
            String description = episode.getDescription();
            inputEpisodes.add(new EpisodeInput(name, duration, description));
        }
        return inputEpisodes;
    }

}
