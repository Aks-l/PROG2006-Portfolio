async function main() {
    // Get contract factories
    const FundManagerFactory = await ethers.getContractFactory("FundManager");
    const ValidatorMultiSigFactory = await ethers.getContractFactory("ValidatorMultiSig");
    const DeveloperPayoutFactory = await ethers.getContractFactory("DeveloperPayout");
  
    // Deploy FundManager
    const fundManager = await FundManagerFactory.deploy();
    await fundManager.waitForDeployment();
    console.log("FundManager deployed to:", await fundManager.getAddress());
  
    // Deploy ValidatorMultiSig
    const validatorMultiSig = await ValidatorMultiSigFactory.deploy();
    await validatorMultiSig.waitForDeployment();
    console.log("ValidatorMultiSig deployed to:", await validatorMultiSig.getAddress());
  
    // Deploy DeveloperPayout
    const developerPayout = await DeveloperPayoutFactory.deploy();
    await developerPayout.waitForDeployment();
    console.log("DeveloperPayout deployed to:", await developerPayout.getAddress());
  }
  
  main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
  