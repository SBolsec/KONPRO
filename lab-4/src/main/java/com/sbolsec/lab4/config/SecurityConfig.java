package com.sbolsec.lab4.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http.csrf()
                .disable()
                .authorizeRequests()
                .anyRequest().authenticated()
                .and()
                .formLogin();

        return http.build();
    }

    @Bean
    public InMemoryUserDetailsManager userDetailsManager() {
        final UserDetails user1 = User.withUsername("user1").roles("USER").password(passwordEncoder().encode("password")).build();
        final UserDetails user2 = User.withUsername("user2").roles("USER").password(passwordEncoder().encode("password")).build();
        final UserDetails user3 = User.withUsername("user3").roles("USER").password(passwordEncoder().encode("password")).build();
        final UserDetails user4 = User.withUsername("user4").roles("USER").password(passwordEncoder().encode("password")).build();
        final UserDetails user5 = User.withUsername("user5").roles("USER").password(passwordEncoder().encode("password")).build();

        return new InMemoryUserDetailsManager(user1, user2, user3, user4, user5);
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

}
