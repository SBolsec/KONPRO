package com.sbolsec.lab4.service.impl;

import com.sbolsec.lab4.dto.Programme;
import com.sbolsec.lab4.dto.Tv;
import com.sbolsec.lab4.service.TvProgramExchangeService;
import com.sbolsec.lab4.service.TvService;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TvServiceImpl implements TvService {

    private final TvProgramExchangeService tvProgramExchangeService;

    private final Map<String, Long> subscriptions = Collections.synchronizedMap(new HashMap<>());

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
    public Long getRemainingTime(String username) {
        final Long programmeId = subscriptions.get(username);
        final Programme programme = programmeIdMap.get(programmeId);

        return ChronoUnit.MINUTES.between(LocalDateTime.now(), programme.getStart());
    }

}
