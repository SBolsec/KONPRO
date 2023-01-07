package com.sbolsec.dz04.controller;

import com.sbolsec.dz04.controller.response.RemainingTimeResponse;
import com.sbolsec.dz04.service.TvService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import java.security.Principal;

@RestController
@RequiredArgsConstructor
public class TvRestController {

    private final TvService tvService;

    @GetMapping("/remaining-time")
    @ResponseStatus(HttpStatus.OK)
    public DeferredResult<ResponseEntity<RemainingTimeResponse>> getRemainingTime(Principal principal) {
        final DeferredResult<ResponseEntity<RemainingTimeResponse>> result = new DeferredResult<>();

        result.onTimeout(() -> result.setErrorResult(ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)));
        tvService.getRemainingTime(principal.getName(), result);

        return result;
    }

}
