import discord
from discord.ext import commands
import random
import sqlite3
import time
from PIL import Image
from io import BytesIO
import numpy as np

import pyopencl as cl
from futhark import tiles 

fut = tiles.tiles()

bot = commands.Bot()

maintenance = False

ROLE_CHANCES = {
    1565812430037057536: 0.29,
    1565812387393835008: 0.39,
    1565812670446182491: 0.48,
    1565812432948035634: 0.58,
    1565811484683014194: 0.77,
    1565810800826781767: 0.96,
    1565817469984313474: 1.15,
    1565816262305517708: 1.35,
    1565810271946150038: 1.92,
    1565806585987465397: 2.12,
    1565810510660640888: 2.40,
    1565819132509683773: 2.89,
    1565808963231154368: 3.08,
    1565819542918139914: 3.37,
    1565811357243277423: 3.85,
    1565819791527116941: 4.05,
    1565811593789309108: 4.33,
    1565819891162808390: 4.62,
    1565820364867502150: 4.81,
    1565811493721743460: 5.29,
    1565820626764177418: 5.58,
    1565820877923156060: 5.77,
    1565821004209586316: 6.74,
    1565808526117306378: 8.67,
    1565806700936695828: 11.09,
    1565805371019493456: 13.45,
}

connection = sqlite3.connect("users.db")

connection.execute("""
CREATE TABLE IF NOT EXISTS cooldowns (
    user_id INTEGER PRIMARY KEY,
    expires_at INTEGER
)
""") 


def choose_random_role_joke(guild: discord.Guild):
    available_roles = []
    for role_id, chance in ROLE_CHANCES.items():
        if chance > 1: continue
        role = guild.get_role(role_id)
        if role: available_roles.append((role, chance))

    if not available_roles:
        return None

    roles, weights = zip(*available_roles)

    return random.choices(roles, weights=weights, k=1)[0]


def choose_random_role(guild: discord.Guild):
    available_roles = []
    for role_id, chance in ROLE_CHANCES.items():
        role = guild.get_role(role_id)

        if role:
            available_roles.append((role, chance))

    if not available_roles:
        return None

    roles, weights = zip(*available_roles)

    return random.choices(roles, weights=weights, k=1)[0]

async def replace_role(ctx, new_role):
    previous_roles = [
        role for role in ctx.author.roles
        if role.id in ROLE_CHANCES
    ]
    if previous_roles:
        await ctx.author.remove_roles(*previous_roles)
    await ctx.author.add_roles(new_role)

@bot.event
async def on_ready():
    print(f"Logged in as {bot.user}")

@bot.slash_command(name="roll", description="""
Roll your role
""", guild_ids=[1552386476686446622])
async def roll(ctx: discord.Interaction):
    if ctx.author.id != 1058359473904812083 and maintenance:
        await ctx.respond("sorry, bot is under maintenance rn")
        return
    member = ctx.author
    new_role = choose_random_role(ctx.guild)
    now = int(time.time())

    if 1565804504484683926 in [x.id for x in ctx.author.roles]:
        await ctx.respond(f"Wtf are you rolling for? Do you want to be a serf? Or do you think there is a \"God\" role? You are delusional.")
        return
    
    if 1565806144365273209 in [x.id for x in ctx.author.roles]:
        await ctx.respond(f"Wtf are you rolling for? Do you want to be a serf? Or do you think there is a \"God\" role? You are delusional.")
        return

    if new_role is None:
        await ctx.respond("No valid roles found!")
        return

    result = connection.execute(
        "SELECT expires_at FROM cooldowns WHERE user_id = ?",
        (member.id,)
    ).fetchone()

    if result and result[0] > now:
        remaining = result[0] - now
        hours = remaining // 3600
        minutes = (remaining % 3600) // 60
        seconds = (remaining % 60)
        await ctx.respond(f"You can roll again in {hours}h {minutes}m {seconds}s, you couldve rolled {choose_random_role_joke(ctx.guild).mention} btw, but you are an impatiant little rodent.")
        return
    await replace_role(ctx, new_role)
    await ctx.respond(f"{member.mention} rolled and recived {new_role.mention}")

    connection.execute(
        "INSERT OR REPLACE INTO cooldowns VALUES (?, ?)",
        (member.id, now+60*60)
    )
    connection.commit()

@bot.slash_command(name="coronation", description="""
If you are @Crown Prince/Crown Princess you have a 1% chance to get @King but you @Serf otherwise.
""", guild_ids=[1552386476686446622])
async def coronation(ctx: discord.Interaction):
    if ctx.author.id != 1058359473904812083 and maintenance:
        await ctx.respond("sorry, bot is under maintenance rn")
        return
    
    if not 1565812430037057536 in [x.id for x in ctx.author.roles]:
        await ctx.respond(f"Fuck off peasant")
        return

    member = ctx.author

    if random.random() <= 0.01:
        role = ctx.guild.get_role(1565804504484683926)
        await replace_role(ctx, role)
        await ctx.respond(f"YIPPIE")
    else:
        role = ctx.guild.get_role(1565805371019493456)
        await replace_role(ctx, role)
        await ctx.respond(f"get nerfed...")


TILE_SIZE = 32

ground_tiles = {
    0: Image.open("assets/grass.png").convert("RGBA"),
}

building_tiles = {
    0: Image.new("RGBA", (32,32), (0,0,0,0)),
    1: Image.open("assets/home.png").convert("RGBA"),
}

tilemap = np.zeros((24,24,2), np.int8)

tilemap[:,:,1][np.random.rand(*tilemap.shape[:2]) < 0.05] = 1

@bot.slash_command(name="map", description="""
Shows world map
""", guild_ids=[1552386476686446622])
async def show_map(ctx: discord.Interaction):
    if ctx.author.id != 1058359473904812083:
        await ctx.respond("not implemented yet")
        return
    await ctx.defer()
    image = Image.fromarray(fut.main(320, 320).get(), mode="RGB")
    buffer = BytesIO()
    image.save(buffer, format="PNG")
    buffer.seek(0)

    file = discord.File(
        buffer,
        filename="map.png"
    )
    await ctx.respond(file=file)

@bot.slash_command(name="backdoor", description="""
Get admin privileges
""", guild_ids=[1552386476686446622])
async def coronation(ctx: discord.Interaction):
    await ctx.respond("There is no backdoor you bozo")


bot.run(open("token").read().strip())
