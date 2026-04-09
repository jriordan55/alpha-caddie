package com.alphacaddie.web;

import com.alphacaddie.service.DataGolfService;
import com.alphacaddie.service.DataGolfService.OuTableRow;
import com.alphacaddie.service.DataGolfService.PlayerProjection;
import com.alphacaddie.time.DisplayRound;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.Map;

@Controller
public class DashboardController {

    private final DataGolfService dataGolfService;
    private final ObjectMapper objectMapper;

    public DashboardController(DataGolfService dataGolfService, ObjectMapper objectMapper) {
        this.dataGolfService = dataGolfService;
        this.objectMapper = objectMapper;
    }

    @GetMapping("/")
    public String home(
            @RequestParam(defaultValue = "mu") String tab,
            @RequestParam(defaultValue = "Total score") String stat,
            Model model) throws JsonProcessingException {

        int roundNum = DisplayRound.currentRound();
        JsonNode field = dataGolfService.getFieldUpdates();
        JsonNode skillRoot = dataGolfService.getSkillRatings();
        Map<Integer, Double> skills = dataGolfService.parseSkillByDgId(skillRoot);
        List<PlayerProjection> players = dataGolfService.buildProjections(field, skills, roundNum);
        List<OuTableRow> ouRows = dataGolfService.buildOuRows(players, stat);
        int[] lineHeaders = lineHeadersForStat(stat);
        boolean showTeeTime = false;
        for (OuTableRow r : ouRows) {
            if (r.getTeeTime() != null && !r.getTeeTime().trim().isEmpty() && !"—".equals(r.getTeeTime())) {
                showTeeTime = true;
                break;
            }
        }

        String eventSubtitle = dataGolfService.eventName(field);
        String cn = dataGolfService.courseName(field);
        if (cn != null && !cn.trim().isEmpty()) {
            eventSubtitle = eventSubtitle + " · " + cn;
        }

        model.addAttribute("tab", tab);
        model.addAttribute("stat", stat);
        model.addAttribute("hasKey", dataGolfService.hasApiKey());
        model.addAttribute("eventName", dataGolfService.eventName(field));
        model.addAttribute("courseName", dataGolfService.courseName(field));
        model.addAttribute("eventSubtitle", eventSubtitle);
        model.addAttribute("roundNum", roundNum);
        model.addAttribute("roundLabel", DisplayRound.roundLabel(roundNum) + " (auto, America/New_York)");
        model.addAttribute("ouRows", ouRows);
        model.addAttribute("lineHeaders", lineHeaders);
        model.addAttribute("showTeeTime", showTeeTime);
        model.addAttribute("players", players);
        model.addAttribute("matchupsJson", prettyJson(dataGolfService.getMatchups("round_matchups")));
        model.addAttribute("outrightsJson", prettyJson(dataGolfService.getOutrights("win")));

        return "index";
    }

    private String prettyJson(JsonNode node) throws JsonProcessingException {
        if (node == null || node.isNull()) return "(no data — set datagolf.api-key)";
        return objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(node);
    }

    private static int[] lineHeadersForStat(String stat) {
        if ("Birdies".equals(stat) || "Bogeys".equals(stat)) return range(0, 6);
        if ("Pars".equals(stat) || "GIR".equals(stat) || "Fairways hit".equals(stat)) return range(5, 15);
        return range(64, 74);
    }

    private static int[] range(int a, int bInc) {
        int[] x = new int[bInc - a + 1];
        for (int i = 0; i < x.length; i++) x[i] = a + i;
        return x;
    }
}
