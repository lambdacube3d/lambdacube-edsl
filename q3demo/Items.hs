module Items where

data Tag
    = TG_NONE
    -- PowerUp
    | PW_QUAD
    | PW_BATTLESUIT
    | PW_HASTE
    | PW_INVIS
    | PW_REGEN
    | PW_FLIGHT
    | PW_REDFLAG
    | PW_BLUEFLAG
    | PW_NEUTRALFLAG
    | PW_SCOUT
    | PW_GUARD
    | PW_DOUBLER
    | PW_AMMOREGEN
    | PW_INVULNERABILITY
    -- Holdable
    | HI_TELEPORTER
    | HI_MEDKIT
    | HI_KAMIKAZE
    | HI_PORTAL
    | HI_INVULNERABILITY
    -- Weapon
    | WP_GAUNTLET
    | WP_MACHINEGUN
    | WP_SHOTGUN
    | WP_GRENADE_LAUNCHER
    | WP_ROCKET_LAUNCHER
    | WP_LIGHTNING
    | WP_RAILGUN
    | WP_PLASMAGUN
    | WP_BFG
    | WP_GRAPPLING_HOOK
    deriving Show

data ItemType
    = IT_BAD
    | IT_WEAPON             -- EFX: rotate + upscale + minlight
    | IT_AMMO               -- EFX: rotate
    | IT_ARMOR              -- EFX: rotate + minlight
    | IT_HEALTH             -- EFX: static external sphere + rotating internal
    | IT_POWERUP            -- instant on, timer based
                            -- EFX: rotate + external ring that rotates
    | IT_HOLDABLE           -- single use, holdable item
                            -- EFX: rotate + bob
    | IT_PERSISTANT_POWERUP
    | IT_TEAM
    deriving Show

data Item
    = Item
    { itClassName   :: String
    , itPickupSound :: String
    , itWorldModel  :: [String]
    , itIcon        :: String
    , itPickupName  :: String
    , itQuantity    :: Int
    , itType        :: ItemType
    , itTag         :: Tag
    , itPreCaches   :: String
    , itSounds      :: String
    } deriving Show

items =
    [ Item
        "item_armor_shard"  
        "sound/misc/ar1_pkup.wav"
        ["models/powerups/armor/shard.md3", "models/powerups/armor/shard_sphere.md3"]
        "icons/iconr_shard" 
        "Armor Shard"
        5
        IT_ARMOR
        TG_NONE
        ""
        ""
    , Item
        "item_armor_combat"
        "sound/misc/ar2_pkup.wav"
        ["models/powerups/armor/armor_yel.md3"]
        "icons/iconr_yellow"
        "Armor"
        50
        IT_ARMOR
        TG_NONE
        ""
        ""
    , Item
        "item_armor_body"
        "sound/misc/ar2_pkup.wav"
        ["models/powerups/armor/armor_red.md3"]
        "icons/iconr_red"
        "Heavy Armor"
        100
        IT_ARMOR
        TG_NONE
        ""
        ""
    , Item
        "item_health_small"
        "sound/items/s_health.wav"
        ["models/powerups/health/small_cross.md3", "models/powerups/health/small_sphere.md3"]
        "icons/iconh_green"
        "5 Health"
        5
        IT_HEALTH
        TG_NONE
        ""
        ""
    , Item
        "item_health"
        "sound/items/n_health.wav"
        ["models/powerups/health/medium_cross.md3", "models/powerups/health/medium_sphere.md3"]
        "icons/iconh_yellow"
        "25 Health"
        25
        IT_HEALTH
        TG_NONE
        ""
        ""
    , Item
        "item_health_large"
        "sound/items/l_health.wav"
        ["models/powerups/health/large_cross.md3", "models/powerups/health/large_sphere.md3"]
        "icons/iconh_red"
        "50 Health"
        50
        IT_HEALTH
        TG_NONE
        ""
        ""
    , Item
        "item_health_mega"
        "sound/items/m_health.wav"
        ["models/powerups/health/mega_cross.md3", "models/powerups/health/mega_sphere.md3"]
        "icons/iconh_mega"
        "Mega Health"
        100
        IT_HEALTH
        TG_NONE
        ""
        ""
    , Item
        "weapon_gauntlet"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/gauntlet/gauntlet.md3"]
        "icons/iconw_gauntlet"
        "Gauntlet"
        0
        IT_WEAPON
        WP_GAUNTLET
        ""
        ""
    , Item
        "weapon_shotgun"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/shotgun/shotgun.md3"]
        "icons/iconw_shotgun"
        "Shotgun"
        10
        IT_WEAPON
        WP_SHOTGUN
        ""
        ""
    , Item
        "weapon_machinegun"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/machinegun/machinegun.md3"]
        "icons/iconw_machinegun"
        "Machinegun"
        40
        IT_WEAPON
        WP_MACHINEGUN
        ""
        ""
    , Item
        "weapon_grenadelauncher"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/grenadel/grenadel.md3"]
        "icons/iconw_grenade"
        "Grenade Launcher"
        10
        IT_WEAPON
        WP_GRENADE_LAUNCHER
        ""
        "sound/weapons/grenade/hgrenb1a.wav sound/weapons/grenade/hgrenb2a.wav"
    , Item
        "weapon_rocketlauncher"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/rocketl/rocketl.md3"]
        "icons/iconw_rocket"
        "Rocket Launcher"
        10
        IT_WEAPON
        WP_ROCKET_LAUNCHER
        ""
        ""
    , Item
        "weapon_lightning"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/lightning/lightning.md3"]
        "icons/iconw_lightning"
        "Lightning Gun"
        100
        IT_WEAPON
        WP_LIGHTNING
        ""
        ""
    , Item
        "weapon_railgun"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/railgun/railgun.md3"]
        "icons/iconw_railgun"
        "Railgun"
        10
        IT_WEAPON
        WP_RAILGUN
        ""
        ""
    , Item
        "weapon_plasmagun"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/plasma/plasma.md3"]
        "icons/iconw_plasma"
        "Plasma Gun"
        50
        IT_WEAPON
        WP_PLASMAGUN
        ""
        ""
    , Item
        "weapon_bfg"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/bfg/bfg.md3"]
        "icons/iconw_bfg"
        "BFG10K"
        20
        IT_WEAPON
        WP_BFG
        ""
        ""
    , Item
        "weapon_grapplinghook"
        "sound/misc/w_pkup.wav"
        ["models/weapons2/grapple/grapple.md3"]
        "icons/iconw_grapple"
        "Grappling Hook"
        0
        IT_WEAPON
        WP_GRAPPLING_HOOK
        ""
        ""
    , Item
        "ammo_shells"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/shotgunam.md3"]
        "icons/icona_shotgun"
        "Shells"
        10
        IT_AMMO
        WP_SHOTGUN
        ""
        ""
    , Item
        "ammo_bullets"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/machinegunam.md3"]
        "icons/icona_machinegun"
        "Bullets"
        50
        IT_AMMO
        WP_MACHINEGUN
        ""
        ""
    , Item
        "ammo_grenades"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/grenadeam.md3"]
        "icons/icona_grenade"
        "Grenades"
        5
        IT_AMMO
        WP_GRENADE_LAUNCHER
        ""
        ""
    , Item
        "ammo_cells"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/plasmaam.md3"]
        "icons/icona_plasma"
        "Cells"
        30
        IT_AMMO
        WP_PLASMAGUN
        ""
        ""
    , Item
        "ammo_lightning"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/lightningam.md3"]
        "icons/icona_lightning"
        "Lightning"
        60
        IT_AMMO
        WP_LIGHTNING
        ""
        ""
    , Item
        "ammo_rockets"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/rocketam.md3"]
        "icons/icona_rocket"
        "Rockets"
        5
        IT_AMMO
        WP_ROCKET_LAUNCHER
        ""
        ""
    , Item
        "ammo_slugs"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/railgunam.md3"]
        "icons/icona_railgun"
        "Slugs"
        10
        IT_AMMO
        WP_RAILGUN
        ""
        ""
    , Item
        "ammo_bfg"
        "sound/misc/am_pkup.wav"
        ["models/powerups/ammo/bfgam.md3"]
        "icons/icona_bfg"
        "Bfg Ammo"
        15
        IT_AMMO
        WP_BFG
        ""
        ""
    , Item
        "holdable_teleporter"
        "sound/items/holdable.wav"
        ["models/powerups/holdable/teleporter.md3"]
        "icons/teleporter"
        "Personal Teleporter"
        60
        IT_HOLDABLE
        HI_TELEPORTER
        ""
        ""
    , Item
        "holdable_medkit"
        "sound/items/holdable.wav"
        ["models/powerups/holdable/medkit.md3", "models/powerups/holdable/medkit_sphere.md3"]
        "icons/medkit"
        "Medkit"
        60
        IT_HOLDABLE
        HI_MEDKIT
        ""
        "sound/items/use_medkit.wav"
    , Item
        "item_quad"
        "sound/items/quaddamage.wav"
        ["models/powerups/instant/quad.md3", "models/powerups/instant/quad_ring.md3"]
        "icons/quad"
        "Quad Damage"
        30
        IT_POWERUP
        PW_QUAD
        ""
        "sound/items/damage2.wav sound/items/damage3.wav"
    , Item
        "item_enviro"
        "sound/items/protect.wav"
        ["models/powerups/instant/enviro.md3", "models/powerups/instant/enviro_ring.md3"]
        "icons/envirosuit"
        "Battle Suit"
        30
        IT_POWERUP
        PW_BATTLESUIT
        ""
        "sound/items/airout.wav sound/items/protect3.wav"
    , Item
        "item_haste"
        "sound/items/haste.wav"
        ["models/powerups/instant/haste.md3", "models/powerups/instant/haste_ring.md3"]
        "icons/haste"
        "Speed"
        30
        IT_POWERUP
        PW_HASTE
        ""
        ""
    , Item
        "item_invis"
        "sound/items/invisibility.wav"
        ["models/powerups/instant/invis.md3", "models/powerups/instant/invis_ring.md3"]
        "icons/invis"
        "Invisibility"
        30
        IT_POWERUP
        PW_INVIS
        ""
        ""
    , Item
        "item_regen"
        "sound/items/regeneration.wav"
        ["models/powerups/instant/regen.md3", "models/powerups/instant/regen_ring.md3"]
        "icons/regen"
        "Regeneration"
        30
        IT_POWERUP
        PW_REGEN
        ""
        "sound/items/regen.wav"
    , Item
        "item_flight"
        "sound/items/flight.wav"
        ["models/powerups/instant/flight.md3", "models/powerups/instant/flight_ring.md3"]
        "icons/flight"
        "Flight"
        60
        IT_POWERUP
        PW_FLIGHT
        ""
        "sound/items/flight.wav"
    , Item
        "team_CTF_redflag"
        ""
        ["models/flags/r_flag.md3"]
        "icons/iconf_red1"
        "Red Flag"
        0
        IT_TEAM
        PW_REDFLAG
        ""
        ""
    , Item
        "team_CTF_blueflag"
        ""
        ["models/flags/b_flag.md3"]
        "icons/iconf_blu1"
        "Blue Flag"
        0
        IT_TEAM
        PW_BLUEFLAG
        ""
        ""
    ]
