package com.sbolsec.dz04.service;

import com.sbolsec.dz04.controller.response.RemainingTimeResponse;
import com.sbolsec.dz04.dto.Programme;
import com.sbolsec.dz04.dto.Tv;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.async.DeferredResult;

public interface TvService {

    Tv getTvProgrammes();

    Programme getSubscription(String username);

    void subscribe(String username, Long programmeId);

    void unsubscribe(String username);

    void getRemainingTime(String username, DeferredResult<ResponseEntity<RemainingTimeResponse>> result);

}
