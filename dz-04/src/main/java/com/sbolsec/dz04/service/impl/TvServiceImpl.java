package com.sbolsec.dz04.service.impl;

import com.sbolsec.dz04.controller.response.RemainingTimeResponse;
import com.sbolsec.dz04.dto.Programme;
import com.sbolsec.dz04.dto.Tv;
import com.sbolsec.dz04.service.TvProgramExchangeService;
import com.sbolsec.dz04.service.TvService;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.async.DeferredResult;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TvServiceImpl implements TvService {

    private final TvProgramExchangeService tvProgramExchangeService;

    private final Map<String, Long> subscriptions = Collections.synchronizedMap(new HashMap<>());

    private final Map<String, DeferredResult<ResponseEntity<RemainingTimeResponse>>> clientRemainingTimeMap =
            Collections.synchronizedMap(new HashMap<>());

    private Tv tv;

    private Map<Long, Programme> programmeIdMap;

    @PostConstruct
    private void initialize() {
        final Tv tv = tvProgramExchangeService.getTvProgrammes();

        long id = 1L;
        for (Programme programme : tv.getProgrammeList()) {
            programme.setId(id++);
        }

        this.tv = tv;
        this.programmeIdMap = tv.getProgrammeList().stream().collect(Collectors.toMap(Programme::getId, Function.identity()));
    }

    @Override
    public Tv getTvProgrammes() {
        return this.tv;
    }

    @Override
    public Programme getSubscription(String username) {
        return programmeIdMap.get(subscriptions.get(username));
    }

    @Override
    public void subscribe(String username, Long programmeId) {
        subscriptions.put(username, programmeId);
    }

    @Override
    public void unsubscribe(String username) {
        subscriptions.remove(username);
    }

    @Override
    public void getRemainingTime(String username, DeferredResult<ResponseEntity<RemainingTimeResponse>> result) {
        clientRemainingTimeMap.put(username, result);
    }

    @Scheduled(initialDelay = 5, fixedRate = 10, timeUnit = TimeUnit.SECONDS)
    private void processRequests() {
        final LocalDateTime currentTime = LocalDateTime.now();

        for (final var entry : clientRemainingTimeMap.entrySet()) {
            final DeferredResult<ResponseEntity<RemainingTimeResponse>> result = entry.getValue();

            if (result.isSetOrExpired()) {
                continue;
            }

            final String username = entry.getKey();
            final Long programmeId = subscriptions.get(username);
            final Programme programme = programmeIdMap.get(programmeId);

            final long remainingTimeInMinutes = ChronoUnit.MINUTES.between(currentTime, programme.getStart());
            result.setResult(ResponseEntity.ok(new RemainingTimeResponse(remainingTimeInMinutes)));
        }

        clientRemainingTimeMap.clear();
    }

}
