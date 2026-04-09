package com.alphacaddie.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;

@Configuration
public class WebClientConfig {

    @Bean
    public WebClient dataGolfWebClient(DataGolfProperties props) {
        String base = props.getBaseUrl() == null ? "https://feeds.datagolf.com" : props.getBaseUrl();
        return WebClient.builder()
                .baseUrl(base.replaceAll("/$", ""))
                .build();
    }
}
