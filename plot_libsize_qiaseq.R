# input:    path to summary excel file downloaded from QIASeq analysis web site
# output:   stacked bar plot showing library size for each sample divided by read type
#
# requires: dplyr, forcats, ggplot2, magrittr, readxl, scales, stringr, tidyr

plot_libsize_qiaseq <- function(path_to_summary_file) {
  
  require(ggplot2, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  
  summary_df <- readxl::read_xlsx(path_to_summary_file, sheet = 1) %>% 
    dplyr::filter(`read set` != "total_reads") %>% 
    tidyr::pivot_longer(-1, names_to = "sample", values_to = "count") %>% 
    dplyr::rename(read_type = `read set`) %>% 
    dplyr::mutate(read_type = stringr::str_remove(read_type, "_[rR]eads*"),
                  read_type = forcats::fct_collapse(read_type,
                                                    defective = c("no_adapter", "too_short", "UMI_defective"),
                                                    other = c("notCharacterized_Mappable", "notCharacterized_notMappable",
                                                              "otherRNA")),
                  read_type = forcats::fct_relevel(read_type, 
                                                   "mRNA", "miRNA", "piRNA", "hairpin", 
                                                   "other", "defective", "rRNA", "tRNA"),
                  sample = stringr::str_remove(sample, "_.*"))
  
  summary_df %>% 
    ggplot(aes(reorder(sample, dplyr::desc(count)), count)) +
    geom_col(aes(fill = read_type)) +
    scale_fill_brewer(palette = "Paired") +
    scale_y_continuous(labels = scales::comma, breaks = scales::breaks_extended(10)) +
    # coord_flip() +
    theme_bw() +
    labs(x = "", y = "read count", fill = "Read type") +
    theme(panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())
  
}