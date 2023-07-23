server <- function(input, output, session) {
  
  change_column_names <- function(data) {
    colnames(data) <- colnames(data) %>%
      str_replace_all("[^[:alnum:]_]", "_") %>%
      str_to_lower()
    return(data)
  }
  
  # Membuat daftar zona dalam for loop
  # daftar_zona <- c()
  # for (i in 0:7) {
  #   zona <- ifelse(i == 0, "Semua Zona", paste0("Zona ", i))
  #   daftar_zona <- c(daftar_zona, zona)
  # }
  
  # MEMUAT DATA
  ### RINCIAN ###
  # lokasi_sekolah <- read_csv("E:/Proyekan/PPDB Bogor/Hasil/shiny-datasets/lokasi_sekolah.csv",
  #                            col_types = cols(No = col_skip(), Koordinat = col_skip())) %>% change_column_names()
  # lokasi_sekolah$warna_sekolah <- distinctColorPalette(length(unique(lokasi_sekolah$nama_sekolah)))
  # write.csv(lokasi_sekolah, "E:/Proyekan/PPDB Bogor/Hasil/shiny-datasets/rincian_sekolah.csv")
  rincian_sekolah <- read_csv("E:/Proyekan/PPDB Bogor/Hasil/shiny-datasets/rincian_sekolah.csv", 
                              col_types = cols(...1 = col_skip()))
  ### PRESTASI ###
  smp_prestasi <- read_csv("E:/Proyekan/PPDB Bogor/Hasil/shiny-datasets/smp_prestasi_asal.csv", 
                           col_types = cols(...1 = col_skip(), `Nomor Pendaftaran` = col_character(), 
                                            `Jenis Kejuaraan` = col_character(), 
                                            `Nilai Total` = col_double())) %>% change_column_names()
  smp_prestasi$jenis_kejuaraan <- str_to_upper(smp_prestasi$jenis_kejuaraan)
  smp_prestasi <- drop_na(smp_prestasi)
  warna_prestasi <- data.frame(jenis_kejuaraan = unique(smp_prestasi$jenis_kejuaraan),
                               warna_prestasi = distinctColorPalette(length(unique(smp_prestasi$jenis_kejuaraan))))
  smp_prestasi <- smp_prestasi %>%
    left_join(rincian_sekolah%>%select(nama_sekolah, warna_sekolah), by=c("sekolah_pilihan" = "nama_sekolah")) %>%
    left_join(warna_prestasi, by="jenis_kejuaraan")
  
  ### RAPOR ###
  smp_rapor <- read_csv("E:/Proyekan/PPDB Bogor/Hasil/shiny-datasets/smp_rapor_asal.csv", 
                        col_types = cols(...1 = col_skip(), `Nomor Pendaftaran` = col_character(), 
                                         `Nilai Total` = col_double())) %>% change_column_names()
  smp_rapor <- merge(smp_rapor, rincian_sekolah%>%select(nama_sekolah, warna_sekolah), by.x = "sekolah_pilihan", by.y = "nama_sekolah", all.x = TRUE)
  
  ### ZONASI ###
  smp_zonasi <- read_csv("E:/Proyekan/PPDB Bogor/Hasil/shiny-datasets/smp_zonasi_hasil.csv", 
                         col_types = cols(#...1 = col_skip(),
                                          `Jarak (meter)` = col_double())) %>% change_column_names()
  smp_zonasi <- smp_zonasi %>%
    mutate(
      warna_zona = ifelse(zona == "Zona 1", "#e53935",
                     ifelse(zona == "Zona 2", "#ffb74d",
                            ifelse(zona == "Zona 3", "#ffee58",
                                   ifelse(zona == "Zona 4", "#388e5a",
                                          ifelse(zona == "Zona 5", "#42a5f5",
                                                 ifelse(zona == "Zona 6", "#8756d5",
                                                        ifelse(zona == "Zona 7", "#ea9bd7", "grey")))))))) %>%
    rename(No = 1) %>%
    merge(rincian_sekolah%>%select(nama_sekolah, warna_sekolah), by.x = "pilihan_sekolah", by.y = "nama_sekolah", all.x = TRUE) %>%
    arrange(No)
  
  # MEMBUAT DASHBOARD
  ### PRESTASI ###
  output$jumlahSiswaPrestasi <- renderInfoBox({
    if (input$sekolahInput == "Semua Sekolah") {
      infoBox(
        "Jumlah Siswa Diterima", nrow(smp_prestasi),
        icon = icon("users")
      )
    } else {
      infoBox(
        "Jumlah Siswa Diterima", smp_prestasi %>% filter(sekolah_pilihan == input$sekolahInput) %>% nrow(),
        icon = icon("users")
      )
    }
  })
  output$nilaiTertinggiPrestasi <- renderInfoBox({
    if (input$sekolahInput == "Semua Sekolah") {
      infoBox(
        "Nilai Tertinggi", max(smp_prestasi$nilai_total),
        icon = icon("ranking-star"), color = "green"
      )
    } else {
      infoBox(
        "Nilai Tertinggi", smp_prestasi %>% filter(sekolah_pilihan == input$sekolahInput) %>% select(nilai_total) %>% max(),
        icon = icon("ranking-star"), color = "green"
      )
    }
  })
  output$passingGradePrestasi <- renderInfoBox({
    if (input$sekolahInput == "Semua Sekolah") {
      infoBox(
        "Nilai Terendah", min(smp_prestasi$nilai_total),
        icon = icon("ranking-star"), color = "red"
      )
    } else {
      infoBox(
        "Nilai Terendah", smp_prestasi %>% filter(sekolah_pilihan == input$sekolahInput) %>% select(nilai_total) %>% min(),
        icon = icon("ranking-star"), color = "red"
      )
    }
  })
  output$prestasiTerbanyak <- renderInfoBox({
    if (input$sekolahInput == "Semua Sekolah") {
      infoBox(
        "Prestasi Terbanyak",
        smp_prestasi %>%
          select(jenis_kejuaraan) %>%
          table() %>%
          as.data.frame() %>%
          arrange(desc(Freq)) %>%
          head(1) %>%
          pull(jenis_kejuaraan) %>%
          as.character(),
        icon = icon("trophy"), color = "yellow")
    } else {
      infoBox(
        "Prestasi Terbanyak",
        smp_prestasi %>%
          filter(sekolah_pilihan == input$sekolahInput) %>%
          select(jenis_kejuaraan) %>%
          table() %>%
          as.data.frame() %>%
          arrange(desc(Freq)) %>%
          head(1) %>%
          pull(jenis_kejuaraan) %>%
          as.character(),
        icon = icon("trophy"), color = "yellow")
    }
  })
  output$grafikPrestasi <- renderPlotly({
    if (input$sekolahInput == "Semua Sekolah") {
      top_prestasi <- as.data.frame(table(smp_prestasi$jenis_kejuaraan)) %>% arrange(desc(Freq)) %>% head(10)
    } else {
      top_prestasi <- as.data.frame(table(smp_prestasi$jenis_kejuaraan[smp_prestasi$sekolah_pilihan == input$sekolahInput])) %>% arrange(desc(Freq)) %>% head(10)
    }
    top_prestasi <- merge(top_prestasi, warna_prestasi, by.x="Var1", by.y = "jenis_kejuaraan", all.x = TRUE)
    
    plot1 <- top_prestasi %>% ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
      geom_bar(stat = "identity", aes(fill=Var1), show.legend = FALSE) +
      #geom_text(aes(x=Var1, y=Freq), hjust = 0, nudge_x = -0.25, color = "white") +
      labs(title = paste0(top_prestasi %>% nrow(), " Prestasi Teratas ", input$sekolahInput),
           x = "",
           y = "Jumlah") +
      coord_flip() +
      scale_fill_manual(values = top_prestasi$warna_prestasi) +
      theme_minimal()
    
    ggplotly(plot1) %>%
      config(displayModeBar = F) %>%
      layout(autosize = TRUE, showlegend = FALSE)
  })
  output$sekolahPrestasi <- renderPlotly({
    if (input$sekolahInput == "Semua Sekolah") {
      top_sekolah <- as.data.frame(table(smp_prestasi$asal_sekolah)) %>% arrange(desc(Freq)) %>% head(10)
    } else {
      top_sekolah <- as.data.frame(table(smp_prestasi$asal_sekolah[smp_prestasi$sekolah_pilihan == input$sekolahInput])) %>% arrange(desc(Freq)) %>% head(10)
    }
    
    plot2 <- top_sekolah %>% ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
      geom_bar(stat = "identity", aes(fill=Var1), show.legend = FALSE) +
      labs(title = paste0(top_sekolah %>% nrow(), " Sekolah Asal dengan Jumlah Terbanyak di ", input$sekolahInput),
           x = "",
           y = "Jumlah") +
      coord_flip() +
      scale_fill_manual(values = distinctColorPalette(length(unique(top_sekolah$Var1)))) +
      theme_minimal()
    
    ggplotly(plot2) %>%
      config(displayModeBar = F) %>%
      layout(autosize = TRUE, showlegend = FALSE)
  })
  output$tabelPrestasi <- renderReactable({
    if (input$sekolahInput == "Semua Sekolah") {
      smp_prestasi_table <- smp_prestasi %>% select(nama_siswa, asal_sekolah, sekolah_pilihan, jenis_kejuaraan, nilai_total, warna_sekolah, warna_prestasi)
      reactable(smp_prestasi_table,
                searchable = TRUE,
                theme = clean(),
                columns = list(
                  nama_siswa = colDef(
                    name = "Nama Siswa"
                  ),
                  asal_sekolah = colDef(
                    name = "Asal Sekolah"
                  ),
                  sekolah_pilihan = colDef(
                    name = "Sekolah Pilihan",
                    cell = pill_buttons(data = smp_prestasi_table, color_ref = "warna_sekolah")
                  ),
                  jenis_kejuaraan = colDef(
                    name = "Jenis Kejuaraan",
                    cell = pill_buttons(data = smp_prestasi_table, color_ref = "warna_prestasi")
                  ),
                  nilai_total = colDef(
                    name = "Nilai Total",
                    cell = data_bars(
                      data = smp_prestasi_table,
                      text_position = 'outside-base',
                      number_fmt = scales::label_number(accuracy = 0.1),
                      fill_color = viridis::viridis(5),
                      animation = 'width 0.4s linear'
                    )
                  ),
                  warna_sekolah = colDef(
                    show = FALSE
                  ),
                  warna_prestasi = colDef(
                    show = FALSE
                  )
                ))
    } else {
      smp_prestasi_table <- smp_prestasi %>% filter(sekolah_pilihan == input$sekolahInput) %>% select(nama_siswa, asal_sekolah, sekolah_pilihan, jenis_kejuaraan, nilai_total, warna_sekolah, warna_prestasi)
      reactable(smp_prestasi_table,
                searchable = TRUE,
                theme = clean(),
                columns = list(
                  nama_siswa = colDef(
                    name = "Nama Siswa"
                  ),
                  asal_sekolah = colDef(
                    name = "Asal Sekolah"
                  ),
                  sekolah_pilihan = colDef(
                    name = "Sekolah Pilihan",
                    cell = pill_buttons(data = smp_prestasi_table, color_ref = "warna_sekolah")
                  ),
                  jenis_kejuaraan = colDef(
                    name = "Jenis Kejuaraan",
                    cell = pill_buttons(data = smp_prestasi_table, color_ref = "warna_prestasi")
                  ),
                  nilai_total = colDef(
                    name = "Nilai Total",
                    cell = data_bars(
                      data = smp_prestasi_table,
                      text_position = 'outside-base',
                      number_fmt = scales::label_number(accuracy = 0.1),
                      fill_color = viridis::viridis(5),
                      animation = 'width 0.4s linear'
                    )
                  ),
                  warna_sekolah = colDef(
                    show = FALSE
                  ),
                  warna_prestasi = colDef(
                    show = FALSE
                  )
                ))
    }
  })
  ### RAPOR ###
  output$jumlahSiswaRapor <- renderInfoBox({
    if (input$sekolahInputRapor == "Semua Sekolah") {
      infoBox(
        "Jumlah Siswa Diterima", nrow(smp_rapor),
        icon = icon("users")
      )
    } else {
      infoBox(
        "Jumlah Siswa Diterima", smp_rapor %>% filter(sekolah_pilihan == input$sekolahInputRapor) %>% nrow(),
        icon = icon("users")
      )
    }
  })
  output$nilaiTertinggiRapor <- renderInfoBox({
    if (input$sekolahInputRapor == "Semua Sekolah") {
      infoBox(
        "Nilai Tertinggi", max(smp_rapor$nilai_total),
        icon = icon("graduation-cap"), color = "green"
      )
    } else {
      infoBox(
        "Nilai Tertinggi", smp_rapor %>% filter(sekolah_pilihan == input$sekolahInputRapor) %>% select(nilai_total) %>% max(),
        icon = icon("graduation-cap"), color = "green"
      )
    }
  })
  output$passingGradeRapor <- renderInfoBox({
    if (input$sekolahInputRapor == "Semua Sekolah") {
      infoBox(
        "Passing Grade", min(smp_rapor$nilai_total),
        icon = icon("circle-check"), color = "red"
      )
    } else {
      infoBox(
        "Passing Grade", smp_rapor %>% filter(sekolah_pilihan == input$sekolahInputRapor) %>% select(nilai_total) %>% min(),
        icon = icon("circle-check"), color = "red"
      )
    }
  })
  output$sekolahRapor <- renderPlotly({
    if (input$sekolahInputRapor == "Semua Sekolah") {
      top_sekolah_rapor <- as.data.frame(table(smp_rapor$asal_sekolah)) %>% arrange(desc(Freq)) %>% head(10)
    } else {
      top_sekolah_rapor <- as.data.frame(table(smp_rapor$asal_sekolah[smp_rapor$sekolah_pilihan == input$sekolahInputRapor])) %>% arrange(desc(Freq)) %>% head(10)
    }
    
    plot3 <- top_sekolah_rapor %>% ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
      geom_bar(stat = "identity", aes(fill = Var1), show.legend = FALSE) +
      labs(title = paste0(top_sekolah_rapor %>% nrow(), " Sekolah dengan Pendaftar Terbanyak di ", input$sekolahInputRapor),
           x = "",
           y = "Jumlah") +
      coord_flip() +
      scale_fill_manual(values = distinctColorPalette(length(unique(top_sekolah_rapor$Var1))))
    theme_minimal()
    
    ggplotly(plot3) %>%
      config(displayModeBar = F) %>%
      layout(autosize = TRUE, showlegend = FALSE)
  })
  output$tabelRapor <- renderReactable({
    if (input$sekolahInputRapor == "Semua Sekolah") {
      reactable(smp_rapor %>% select(nama_siswa, asal_sekolah, sekolah_pilihan, nilai_total, warna_sekolah),
                searchable = TRUE,
                theme = clean(),
                columns = list(
                  nama_siswa = colDef(
                    name = "Nama Siswa"
                  ),
                  asal_sekolah = colDef(
                    name = "Asal Sekolah"
                  ),
                  sekolah_pilihan = colDef(
                    name = "Sekolah Pilihan",
                    cell = pill_buttons(data = smp_rapor, color_ref = "warna_sekolah")
                  ),
                  nilai_total = colDef(
                    name = "Nilai Total",
                    style = color_scales(smp_rapor)
                  ),
                  warna_sekolah = colDef(
                    show = FALSE
                  )
                ))
    } else {
      reactable(smp_rapor %>% filter(sekolah_pilihan == input$sekolahInputRapor) %>% select(nama_siswa, asal_sekolah, sekolah_pilihan, nilai_total, warna_sekolah),
                searchable = TRUE,
                theme = clean(),
                columns = list(
                  nama_siswa = colDef(
                    name = "Nama Siswa"
                  ),
                  asal_sekolah = colDef(
                    name = "Asal Sekolah"
                  ),
                  sekolah_pilihan = colDef(
                    name = "Sekolah Pilihan",
                    cell = pill_buttons(data = smp_rapor, color_ref = "warna_sekolah")
                  ),
                  nilai_total = colDef(
                    name = "Nilai Total",
                    style = color_scales(smp_rapor)
                  ),
                  warna_sekolah = colDef(
                    show = FALSE
                  )
                ))
    }
  })
  ### ZONASI ###
  data_zonasi <- reactive({
    temp_data <- smp_zonasi
    
    if (input$sekolahInputZonasi != "Semua Sekolah") {
      temp_data <- temp_data[temp_data$pilihan_sekolah == input$sekolahInputZonasi, ]
    }
    
    if (input$zonasiInput != "Semua Zona") {
      temp_data <- temp_data[temp_data$zona == input$zonasiInput, ]
    }
    
    if (input$zonasiStatus != "Semua") {
      temp_data <- temp_data[temp_data$hasil == input$zonasiStatus, ]
    }
    
    return(temp_data)
  })
  # Mempebaharui angka maksimal pada slider
  observe({
    updateSliderInput(session, "zonasiUrutan",
                      max = nrow(data_zonasi()), value = nrow(data_zonasi()))
  })
  output$jumlahSiswaZonasi <- renderInfoBox({
    infoBox(
      "Jumlah Siswa Zonasi", nrow(data_zonasi()[1:input$zonasiUrutan, ]),
      icon = icon("users")
    )
  })
  output$jumlahTerimaZonasi <- renderInfoBox({
    infoBox(
      "Jumlah Siswa Diterima",
      sum(data_zonasi()[1:input$zonasiUrutan, ]$hasil == "Diterima"),
      icon = icon("users"), color = "green"
    )
  })
  output$jumlahTolakZonasi <- renderInfoBox({
    infoBox(
      "Jumlah Siswa Ditolak",
      sum(data_zonasi()[1:input$zonasiUrutan, ]$hasil == "Ditolak"),
      icon = icon("users-slash"), color = "red"
    )
  })
  output$jarakTerdekatZonasi <- renderInfoBox({
    infoBox(
      "Jarak Terdekat",
      data_zonasi()[1:input$zonasiUrutan, ] %>%
        select(jarak__meter_) %>% min() %>%
        paste("meter"),
      icon = icon("map-location-dot"), color = "purple"
    )
  })
  output$jarakTerjauhZonasi <- renderInfoBox({
    infoBox(
      "Jarak Terjauh",
      data_zonasi()[1:input$zonasiUrutan, ] %>%
        select(jarak__meter_) %>% max() %>%
        paste("meter"),
      icon = icon("map-location-dot"), color = "maroon"
    )
  })
  output$sekolahTeratasZonasi <- renderInfoBox({
    infoBox(
      "Asal Sekolah Terbanyak",
      data_zonasi() %>%
        select(asal_sekolah) %>%
        table() %>%
        as.data.frame() %>%
        arrange(desc(Freq)) %>%
        head(1) %>%
        pull(asal_sekolah) %>%
        as.character(),
      icon = icon("school"), color = "yellow"
    )
  })
  output$petaZonasi <- renderLeaflet({
    df_peta <- data_zonasi()[1:input$zonasiUrutan, ] %>%
      select(pilihan_sekolah, zona, jarak__meter_, warna_zona) %>%
      group_by(
        if (input$sekolahInputZonasi == "Semua Sekolah") {
          pilihan_sekolah
        } else if (input$zonasiInput == "Semua Zona") {
          zona
        } else {
          NULL
        }
      ) %>%
      slice_max(jarak__meter_) %>%
      merge(rincian_sekolah%>%select(-image), by.x = 'pilihan_sekolah', by.y = 'nama_sekolah', all.x = TRUE)
    
    leaflet(df_peta) %>%
      addTiles() %>%
      addMarkers(
        ~longitude,
        ~latitude,
        popup = ~paste(pilihan_sekolah, "<br>", zona, "<br>", jarak__meter_, "meter")
      ) %>%
      addCircles(
        radius = ~jarak__meter_,
        color = ~warna_zona,
        fillOpacity = 0.3,
        label = ~paste(zona, ":", jarak__meter_, "meter")
      ) %>%
      setView(lng = mean(df_peta$longitude), lat = mean(df_peta$latitude), zoom = 13)
  })
  output$persentaseZona <- renderPlotly({
    persentase_zona <- data_zonasi()[1:input$zonasiUrutan, ] %>%
      select(zona) %>%
      table() %>%
      as.data.frame() %>%
      mutate(
        warna = ifelse(zona == "Zona 1", "#e53935",
                       ifelse(zona == "Zona 2", "#ffb74d",
                              ifelse(zona == "Zona 3", "#ffee58",
                                     ifelse(zona == "Zona 4", "#388e5a",
                                            ifelse(zona == "Zona 5", "#42a5f5",
                                                   ifelse(zona == "Zona 6", "#8756d5",
                                                          ifelse(zona == "Zona 7", "#ea9bd7", "grey"))))))))
    
    fig_zona <- plot_ly(persentase_zona, labels = ~zona, values = ~Freq, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        hoverinfo = 'text',
                        text = ~paste(Freq, 'orang'),
                        marker = list(colors = ~warna,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE)
    fig_zona
  })
  output$topSekolahZonasiZona <- renderPlotly({
    top_sekolah_zona <- data_zonasi()[1:input$zonasiUrutan, ] %>%
      select(asal_sekolah, zona) %>%
      table() %>%
      as.data.frame() %>%
      pivot_wider(names_from = zona, values_from = Freq, values_fill = 0) %>%
      change_column_names() %>%
      arrange(desc(if (input$zonasiInput == "Semua Zona") {zona_1} else {get(input$zonasiInput %>% str_replace_all("[^[:alnum:]_]", "_") %>% str_to_lower())})) %>%
      head(15)
    
    # Mengatur warna untuk zona tertentu berdasarkan kode warna yang telah ditentukan
    zona_colors <- c("#e53935", "#ffb74d", "#ffee58", "#388e5a", "#42a5f5", "#8756d5", "#ea9bd7")
    
    if (input$zonasiInput == "Semua Zona") {
      plot_ly(top_sekolah_zona, y = ~asal_sekolah, x = ~zona_1,
              type = 'bar', name = 'Zona 1', orientation = 'h', marker = list(color = zona_colors[1])) %>%
        add_trace(x = ~zona_2, name = 'Zona 2', orientation = 'h', marker = list(color = zona_colors[2])) %>%
        add_trace(x = ~zona_3, name = 'Zona 3', orientation = 'h', marker = list(color = zona_colors[3])) %>%
        add_trace(x = ~zona_4, name = 'Zona 4', orientation = 'h', marker = list(color = zona_colors[4])) %>%
        add_trace(x = ~zona_5, name = 'Zona 5', orientation = 'h', marker = list(color = zona_colors[5])) %>%
        add_trace(x = ~zona_6, name = 'Zona 6', orientation = 'h', marker = list(color = zona_colors[6])) %>%
        add_trace(x = ~zona_7, name = 'Zona 7', orientation = 'h', marker = list(color = zona_colors[7])) %>%
        layout(
          xaxis = list(title = 'Jumlah Siswa'),
          yaxis = list(title = 'Sekolah', categoryorder = "total ascending"),
          barmode = 'stack',
          title = "Statistik Zona Siswa",
          legend = list(title = "Hasil Seleksi Zona", orientation = 'h',
                        xanchor = 'center')
        )
    } else {
      # Mengambil nama kolom yang sesuai untuk zona yang dipilih
      selected_zona <- paste0(input$zonasiInput %>% str_replace_all("[^[:alnum:]_]", "_") %>% str_to_lower())
      selected_color <- zona_colors[as.numeric(substr(input$zonasiInput, start = 6, stop = 6))]
      
      plot_ly(top_sekolah_zona, y = ~asal_sekolah, x = ~get(selected_zona),
              type = 'bar', name = input$zonasiInput, orientation = 'h', marker = list(color = selected_color)) %>%
        layout(
          xaxis = list(title = 'Jumlah Siswa'),
          yaxis = list(title = 'Sekolah', categoryorder = "total ascending"),
          #barmode = 'stack',
          title = "Statistik Zona Siswa"
        )
    }
  })
  output$topSekolahZonasiStatus <- renderPlotly({
    top_sekolah_zonasi <- data_zonasi()[1:input$zonasiUrutan, ] %>%
      select(asal_sekolah, hasil) %>%
      table() %>%
      as.data.frame() %>%
      pivot_wider(names_from = hasil, values_from = Freq, values_fill = 0) %>%
      arrange(desc(if (input$zonasiStatus == "Ditolak") {Ditolak} else {Diterima})) %>%
      head(10)
    
    if (input$zonasiStatus == "Diterima") {
      plot_ly(top_sekolah_zonasi, y = ~asal_sekolah, x = ~Diterima,
              type = 'bar', orientation = 'h', marker = list(color = 'green')) %>%
        layout(
          xaxis = list(title = 'Jumlah Siswa'),
          yaxis = list(title = 'Sekolah', categoryorder = "total ascending")
          #barmode = 'stack',
          #title = "Statistik Pendaftaran Siswa"
        )
    } else if (input$zonasiStatus == "Ditolak") {
      plot_ly(top_sekolah_zonasi, y = ~asal_sekolah, x = ~Ditolak,
              type = 'bar', orientation = 'h', marker = list(color = 'red')) %>%
        layout(
          xaxis = list(title = 'Jumlah Siswa'),
          yaxis = list(title = 'Sekolah', categoryorder = "total ascending")
          #barmode = 'stack',
          #title = "Statistik Pendaftaran Siswa"
        )
    } else {
      plot_ly(top_sekolah_zonasi, y = ~asal_sekolah, x = ~Diterima,
              type = 'bar', name = 'Diterima', orientation = 'h', marker = list(color = 'green')) %>%
        add_trace(x = ~Ditolak, name = 'Ditolak', orientation = 'h', marker = list(color = 'red')) %>%
        layout(
          xaxis = list(title = 'Jumlah Siswa'),
          yaxis = list(title = 'Sekolah', categoryorder = "total ascending"),
          barmode = 'stack',
          #title = "Statistik Pendaftaran Siswa",
          legend = list(title = "Hasil Seleksi", orientation = 'h',
                        xanchor = 'center')
        )
    }
  })
  output$tabelZonasi <- renderReactable({
    tabel_zonasi <- data_zonasi()[1:input$zonasiUrutan, ] %>%
      select(nama_siswa, asal_sekolah, pilihan_sekolah, zona, jarak__meter_, hasil, warna_zona, warna_sekolah)
    
    reactable(tabel_zonasi,
              searchable = TRUE,
              theme = clean(),
              rowStyle = function(index) {
                if (data_zonasi()[index, "hasil"] == "Diterima") {
                  list(background = "rgba(56, 142, 90, 0.5)")
                } else if (data_zonasi()[index, "hasil"] == "Ditolak") {
                  list(background = "rgba(229, 57, 53, 0.5)")
                } else {
                  NULL
                }
              },
              columns = list(
                nama_siswa = colDef(
                  name = "Nama Siswa"
                ),
                asal_sekolah = colDef(
                  name = "Asal Sekolah"
                ),
                pilihan_sekolah = colDef(
                  name = "Pilihan Sekolah",
                  cell = pill_buttons(data = tabel_zonasi, color_ref = "warna_sekolah") 
                ),
                zona = colDef(
                  name = "Zona",
                  cell = pill_buttons(data = tabel_zonasi, color_ref = "warna_zona")
                ),
                jarak__meter_ = colDef(
                  name = "Jarak (meter)",
                  style = color_scales(tabel_zonasi, colors = viridis::viridis(10))
                ),
                hasil = colDef(
                  show = FALSE
                ),
                warna_zona = colDef(
                  show = FALSE
                ),
                warna_sekolah = colDef(
                  show = FALSE
                )
              )) %>%
      add_legend(tabel_zonasi, col_name = "jarak__meter_", title = "Jarak", footer = "Diambil: 17 Juli 2023", colors = viridis::viridis(10))
  })
}

#shinyApp(ui, server)