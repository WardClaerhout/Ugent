library(ggplot2)
library(dplyr)

start_time <- Sys.time()


# Gegevens
Q = 566.67 # m3/min
eta = 0.9975
Ns = 4

# Assumptie-intervallen
w_interval = c(1,10)
D_interval = c(0.15, 0.4)
H_interval = c(4,6)
L_interval = c(2,3)

# Parameter-intervallen
SCA_interval <- c(0.25, 2.1)
R_interval <- c(1,1.5)
u_interval <- c(1.2,2.5)
Pd_interval <- c(1,2)
PcQ_interval <- c(1.75,17.5)
IcA_interval <- c(50,750)

# Functie om mogelijke combinaties te genereren
generate_combinations <- function() {
  combinations <- expand.grid(
    w = seq(w_interval[1], w_interval[2], by = 0.1),
    D = seq(D_interval[1], D_interval[2], by = 0.01),
    H = seq(H_interval[1], H_interval[2], by = 0.1),
    L = seq(L_interval[1], L_interval[2], by = 0.1)
  )
  return(combinations)
}

# Functie om parameters te berekenen op basis van assumpties
calculate_parameters <- function(w, D, H, L) {
  Areq = (-Q/w)*log(1-eta)
  Ap = 2*L*H
  Nx = Areq/Ap + Ns
  Ny = ceiling(Nx)
  N = ifelse(Ny %% 2 == 0, Ny, Ny + 1)
  R = Ns*L/H
  Aact = Ap*(N-Ns)
  Nd = N/Ns-1
  u = Q/(Nd*D*H)/60
  SCA = Aact/Q
  Pc = 0.32*Q*(1/0.3048)^3
  Pd = Pc/Aact/10.76
  w_e = 0.5*Pd*60*0.3048
  PcQ = Pc/Q
  IcA = Pc/30000/Aact*10^6
  ANs = Aact/Ns
  
  
  # Retourneer de berekende parameters
  return(list(SCA = SCA, R = R, u = u, Pd = Pd, PcQ = PcQ, IcA = IcA, ANs = ANs))
}

# Genereer alle mogelijke combinaties
combinations <- generate_combinations()

# Loop door alle combinaties en controleer of parameters binnen de intervallen liggen
valid_combinations <- list()
for (i in 1:nrow(combinations)) {
  params <- calculate_parameters(combinations[i, "w"], combinations[i, "D"], combinations[i, "H"], combinations[i, "L"])
  if (
    SCA_interval[1] <= params$SCA && params$SCA <= SCA_interval[2] &&
    R_interval[1] <= params$R && params$R <= R_interval[2] &&
    u_interval[1] <= params$u && params$u <= u_interval[2] &&
    Pd_interval[1] <= params$Pd && params$Pd <= Pd_interval[2] &&
    PcQ_interval[1] <= params$PcQ && params$PcQ <= PcQ_interval[2] &&
    IcA_interval[1] <= params$IcA && params$IcA <= IcA_interval[2]
  ) {
    valid_combinations <- c(valid_combinations, list(combinations[i, ]))
  }
}

# Toon geldige combinaties
print(valid_combinations)

end_time <- Sys.time()
# Calculate elapsed time
elapsed_time <- end_time - start_time
cat("Elapsed Time:", format(elapsed_time, units = "secs"), "\n")


# Functie om parameters te berekenen op basis van assumpties
calculate_ANs <- function(w, D, H, L) {
  Areq = (-Q/w)*log(1-eta)
  Ap = 2*L*H
  Nx = Areq/Ap + Ns
  Ny = ceiling(Nx)
  N = ifelse(Ny %% 2 == 0, Ny, Ny + 1)
  R = Ns*L/H
  Aact = Ap*(N-Ns)
  Nd = N/Ns-1
  u = Q/(Nd*D*H)/60
  SCA = Aact/Q
  Pc = 0.32*Q*(1/0.3048)^3
  Pd = Pc/Aact/10.76
  w_e = 0.5*Pd*60*0.3048
  PcQ = Pc/Q
  IcA = Pc/30000/Aact*10^6
  ANs = Aact/Ns
  
  # Controleer op NaN of oneindige waarden en zet deze om naar NA
  if (is.finite(ANs)) {
    return(ANs)
  } else {
    return(NA)
  }
}

# Maak een dataframe van de geldige combinaties
valid_combinations_df <- do.call(rbind, valid_combinations)

# Voeg logische kolomnamen toe
logical_column_names <- c("w", "D", "H", "L")
logical_column_names <- expand.grid(
  w = logical_column_names,
  D = logical_column_names,
  H = logical_column_names,
  L = logical_column_names
)$Var1  # Gebruik Var1 als logische kolomnamen

# Zet de kolomnamen van het dataframe naar logische kolomnamen
colnames(valid_combinations_df) <- c("w", "D", "H", "L")

# Voeg een kolom toe voor de ANs parameter
valid_combinations_df$ANs <- apply(valid_combinations_df, 1, function(row) {
  calculate_ANs(row["w"], row["D"], row["H"], row["L"])
})

#orden dataframe van hoge ANs naar lage ANs
valid_combinations_df <- arrange(valid_combinations_df, desc(ANs))

# Toon het resulterende dataframe
print("Dataframe van Geldige Combinaties:")
print(valid_combinations_df)