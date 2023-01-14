package com.sbolsec.lab4.controller;

import com.sbolsec.lab4.controller.request.SubscribeRequest;
import com.sbolsec.lab4.dto.Programme;
import com.sbolsec.lab4.dto.Tv;
import com.sbolsec.lab4.service.TvService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.security.Principal;

@Controller
@RequiredArgsConstructor
public class HomeController {

    private final TvService tvService;

    @GetMapping("/")
    public String home(Model model, Principal principal) {
        if (tvService.getSubscription(principal.getName()) != null) {
            return "redirect:/view-subscription";
        }

        final Tv tv = tvService.getTvProgrammes();

        model.addAttribute("channel", tv.getChannel());
        model.addAttribute("programmes", tv.getProgrammeList());
        model.addAttribute("username", principal.getName());

        return "index";
    }

    @GetMapping("/view-subscription")
    public String viewSubscription(Model model, Principal principal) {
        final Programme programme = tvService.getSubscription(principal.getName());

        if (programme == null) {
            return "redirect:/";
        }

        model.addAttribute("programme", programme);
        model.addAttribute("username", principal.getName());

        return "view-subscription";
    }

    @PostMapping("/subscribe")
    public String subscribe(@ModelAttribute SubscribeRequest request, Principal principal) {
        tvService.subscribe(principal.getName(), request.getProgrammeId());

        return "redirect:/view-subscription";
    }

    @PostMapping("/unsubscribe")
    public String unsubscribe(Principal principal) {
        tvService.unsubscribe(principal.getName());

        return "redirect:/";
    }

}
