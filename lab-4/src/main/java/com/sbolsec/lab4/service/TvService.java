package com.sbolsec.lab4.service;

import com.sbolsec.lab4.dto.Programme;
import com.sbolsec.lab4.dto.Tv;

public interface TvService {

    Tv getTvProgrammes();

    Programme getSubscription(String username);

    void subscribe(String username, Long programmeId);

    void unsubscribe(String username);

    Long getRemainingTime(String username);

}
